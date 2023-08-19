module lmpl4d.unpacker;

import lmpl4d.common;

struct Unpacker(Stream = const(ubyte)[]) if (isInputBuffer!(Stream, ubyte)) {
	Stream buf;
	size_t pos;
	this(Stream stream) {
		buf = stream;
	}

	version (D_Exceptions) {
		void rollback(size_t size, string expected, Format actual = Format.NONE) {
			import std.conv : text;

			pos -= size + 1;
			throw new MessagePackException(text("Attempt to unpack with non-compatible type: ",
					actual ? text("expected = ", expected, ", got = ", actual) : expected));
		}

		void check(size_t size = 1) {
			if (!canRead(size))
				throw new UnpackException("Insufficient buffer");
		}
	} else {
		void rollback(size_t size, string expected, Format actual = Format.NONE) {
			pos -= size + 1;
		}

		void check(size_t size = 1) {
		}
	}

	T unpack(T)()
	if (is(Unqual!T == enum) || isPointer!T || isSomeChar!T ||
		isNumeric!T || is(Unqual!T == bool)) {
		static if (is(Unqual!T == enum))
			return cast(T)unpack!(OriginalType!T);
		else static if (isPointer!T) {
			T val;
			version (D_Exceptions) {
				if (!unpackNil(val))
					throw new UnpackException("Can't deserialize a pointer that is not null");
			}
			return val;
		} else static if (is(Unqual!T == char))
			return cast(T)unpack!ubyte;
		else static if (is(Unqual!T == wchar))
			return cast(T)unpack!ushort;
		else static if (is(Unqual!T == dchar))
			return cast(T)unpack!uint;
		else static if (isNumeric!T || is(Unqual!T == bool)) {
			check();
			int header = read();
			static if (isIntegral!T) {
				if (header <= 0x7f)
					return cast(T)header;
				if (0xe0 <= header && header <= 0xff)
					return cast(byte)header;
			}
			switch (header) {
				static if (is(Unqual!T == bool)) {
			case Format.TRUE:
					return true;
			case Format.FALSE:
					return false;
				} else static if (isIntegral!T) {
			case Format.UINT8:
					check(ubyte.sizeof);
					return read();
			case Format.UINT16:
					check(ushort.sizeof);
					auto val = read!ushort();
					if (val > T.max)
						rollback(ushort.sizeof, T.stringof, Format.UINT16);
					return cast(T)val;
			case Format.UINT32:
					check(uint.sizeof);
					auto val = read!uint();
					if (val > T.max)
						rollback(uint.sizeof, T.stringof, Format.UINT32);
					return cast(T)val;
			case Format.UINT64:
					check(ulong.sizeof);
					auto val = read!ulong();
					if (val > T.max)
						rollback(ulong.sizeof, T.stringof, Format.UINT64);
					return cast(T)val;
			case Format.INT8:
					check(byte.sizeof);
					return cast(T)read();
			case Format.INT16:
					check(short.sizeof);
					auto val = read!short();
					if (val < T.min || T.max < val)
						rollback(short.sizeof, T.stringof, Format.INT16);
					return cast(T)val;
			case Format.INT32:
					check(int.sizeof);
					auto val = read!int();
					if (val < T.min || T.max < val)
						rollback(int.sizeof, T.stringof, Format.INT32);
					return cast(T)val;
			case Format.INT64:
					check(long.sizeof);
					auto val = read!long();
					if (val < T.min || T.max < val)
						rollback(long.sizeof, T.stringof, Format.INT64);
					return cast(T)val;
				} else static if (isFloatingPoint!T) {
			case Format.FLOAT:
					_f val;
					check(uint.sizeof);
					val.i = read!uint();
					return val.f;
			case Format.DOUBLE:
					// check precision loss
					static if (is(Unqual!T == float))
						rollback(0, T.stringof, Format.DOUBLE);

					_d val;
					check(ulong.sizeof);
					val.i = read!ulong();
					return val.f;
			case Format.REAL:
					version (EnableReal) {
						// check precision loss
						static if (is(Unqual!T == float) || is(Unqual!T == double))
							rollback(0, T.stringof, Format.REAL);
						check(real.sizeof);
						version (NonX86) {
							CustomFloat!80 tmp;

							const frac = read!ulong();
							const exp = read!ushort();

							tmp.significand = frac;
							tmp.exponent = exp & 0x7fff;
							tmp.sign = (exp & 0x8000) != 0;

							// NOTE: tmp.get!real is inf on non-x86 when deserialized value is larger than double.max.
							return tmp.get!real;
						} else {
							_r tmp;

							tmp.fraction = read!(typeof(tmp.fraction))();
							tmp.exponent = read!(typeof(tmp.exponent))();

							return tmp.f;
						}
					} else {
						rollback(0, "real is disabled", Format.REAL);
						break;
					}
				}
			default:
				break;
			}
			rollback(0, T.stringof, cast(Format)header);
		}
		assert(0, "Unsupported type");
	}

	T unpack(T)(T defValue) nothrow
	if (is(Unqual!T == enum) || isPointer!T || isTuple!T
		|| isSomeChar!T || isNumeric!T || is(Unqual!T == bool)) {
		static if (is(Unqual!T == enum))
			return cast(T)unpack(cast(OriginalType!T)defValue);
		else static if (isPointer!T) {
			T val;
			return unpackNil(val) ? val : defValue;
		} else static if (isTuple!T) {
			T val;
			unpackArray!(T.Types)(val.field);
			return val;
		} else static if (is(Unqual!T == char))
			return cast(T)unpack(cast(ubyte)defValue);
		else static if (is(Unqual!T == wchar))
			return cast(T)unpack(cast(ushort)defValue);
		else static if (is(Unqual!T == dchar))
			return cast(T)unpack(cast(uint)defValue);
		else static if (isNumeric!T || is(Unqual!T == bool)) {
			if (!canRead)
				return defValue;
			const spos = pos;
			int header = read();
			static if (isIntegral!T) {
				if (header <= 0x7f)
					return cast(T)header;
				if (0xe0 <= header && header <= 0xff)
					return cast(byte)header;
			}
			switch (header) {
				static if (is(Unqual!T == bool)) {
			case Format.TRUE:
					return true;
			case Format.FALSE:
					return false;
				} else static if (isIntegral!T) {
			case Format.UINT8:
					if (!canRead(ubyte.sizeof))
						return defValue;
					return read();
			case Format.UINT16:
					if (!canRead(ushort.sizeof))
						return defValue;
					auto val = read!ushort();
					if (val > T.max)
						return defValue;
					return cast(T)val;
			case Format.UINT32:
					if (!canRead(uint.sizeof))
						return defValue;
					auto val = read!uint();
					if (val > T.max)
						return defValue;
					return cast(T)val;
			case Format.UINT64:
					if (!canRead(ulong.sizeof))
						return defValue;
					auto val = read!ulong();
					if (val > T.max)
						return defValue;
					return cast(T)val;
			case Format.INT8:
					if (!canRead(byte.sizeof))
						return defValue;
					return cast(T)read();
			case Format.INT16:
					if (!canRead(short.sizeof))
						return defValue;
					auto val = read!short();
					if (val < T.min || T.max < val)
						return defValue;
					return cast(T)val;
			case Format.INT32:
					if (!canRead(int.sizeof))
						return defValue;
					auto val = read!int();
					if (val < T.min || T.max < val)
						return defValue;
					return cast(T)val;
			case Format.INT64:
					if (!canRead(long.sizeof))
						return defValue;
					auto val = read!long();
					if (val < T.min || T.max < val)
						return defValue;
					return cast(T)val;
				} else static if (isFloatingPoint!T) {
			case Format.FLOAT:
					_f val;
					if (!canRead(uint.sizeof))
						return defValue;
					val.i = read!uint();
					return val.f;
			case Format.DOUBLE:
					// check precision loss
					static if (is(Unqual!T == float))
						return defValue;
					else {
						_d val = void;
						if (!canRead(ulong.sizeof))
							return defValue;
						val.i = read!ulong();
						return val.f;
					}
			case Format.REAL:
					version (EnableReal) {
						// check precision loss
						static if (is(Unqual!T == float) || is(Unqual!T == double))
							return defValue;
						if (!canRead(real.sizeof))
							return defValue;
						version (NonX86) {
							CustomFloat!80 tmp;

							const frac = read!ulong();
							const exp = read!ushort();

							tmp.significand = frac;
							tmp.exponent = exp & 0x7fff;
							tmp.sign = (exp & 0x8000) != 0;

							// NOTE: tmp.get!real is inf on non-x86 when deserialized value is larger than double.max.
							return tmp.get!real;
						} else {
							_r tmp = void;

							tmp.fraction = read!(typeof(tmp.fraction))();
							tmp.exponent = read!(typeof(tmp.exponent))();

							return tmp.f;
						}
					} else
						return defValue;
				}
			default:
				pos = spos;
				return defValue;
			}
		}
	}

	/// ditto
	ref typeof(this) unpack(Types...)(ref Types objects) if (Types.length > 1) {
		foreach (i, T; Types)
			objects[i] = unpack!T;
		return this;
	}

	T unpack(T)() if (isSomeArray!T) {
		if (checkNil()) {
			static if (isStaticArray!T) {
				pos++;
				rollback(0, "static array", Format.NIL);
			} else {
				T array = void;
				unpackNil(array);
				return array;
			}
		}
		alias U = typeof(T.init[0]);
		enum RawBytes = isRawByte!U;
		static if (RawBytes)
			long length = beginRaw();
		else
			long length = beginArray();
		version (D_Exceptions) {
			import std.conv : text;

			if (length < 0)
				throw new MessagePackException(
					"Attempt to unpack with non-compatible type or buffer is insufficient: expected = array");
			static if (isStaticArray!T)
				if (length != T.length)
					throw new MessagePackException(text("Static array length mismatch: got = ", length,
							"expected = ", T.length));
			static if (__traits(compiles, buf.length))
				if (pos + length > buf.length)
					throw new MessagePackException(text("Invalid array size in byte stream: Length (", length,
							") is larger than internal buffer size (", buf.length, ")"));
		} else {
			if (length < 0)
				return T.init;
			static if (isStaticArray!T) {
				if (length != T.length)
					return T.init;
			}
		}
		static if (isStaticArray!T)
			T array = void;
		else version (D_BetterC)
				static assert(0, "Dynamic array unpacking is not supported in BetterC mode");
			else {
				import std.array;

				T array = uninitializedArray!T(length);
			}
		if (length == 0)
			return array;
		static if (RawBytes) {
			check(length);
			static if (isStaticArray!T)
				array = (cast(U[])read(length))[0 .. T.length];
			else
				array = cast(T)read(length);
		} else
			foreach (ref a; array)
				a = unpack!U;
		return array;
	}

	bool unpack(T)(ref T array) nothrow if (isSomeArray!T) {

		alias U = typeof(T.init[0]);
		const spos = pos;
		if (checkNil()) {
			static if (isStaticArray!T)
				return false;
			else
				return unpackNil(array);
		}
		enum RawBytes = isRawByte!U;
		static if (RawBytes)
			long length = beginRaw();
		else
			long length = beginArray();
		if (length < 0)
			return false;
		static if (__traits(compiles, buf.length))
			if (length > buf.length) {
				pos = spos;
				return false;
			}
		static if (isStaticArray!T)
			if (length != T.length) {
				pos = spos;
				return false;
			}
		if (length == 0)
			return true;
		static if (RawBytes) {
			if (!canRead(length)) {
				pos = spos;
				return false;
			}
			static if (isStaticArray!T)
				array = (cast(U[])read(length))[0 .. T.length];
			else
				array = cast(T)read(length);
		} else {
			static if (!isStaticArray!T) {
				if (array.length != length) {
					version (D_BetterC) {
						pos = spos;
						return false;
					} else {
						import std.array;

						array = uninitializedArray!T(length);
					}
				}
			}
			try
				foreach (ref a; array)
					a = unpack!U;
					catch (Exception)
						return false;
		}
		return true;
	}

	/// ditto
	T unpack(T)() if (isAssociativeArray!T) {
		alias K = typeof(T.init.keys[0]),
		V = typeof(T.init.values[0]);
		T array;

		if (unpackNil(array))
			return array;

		long length = beginMap();
		if (length < 0)
			throw new MessagePackException(
				"Attempt to unpack with non-compatible type: expected = map");
		if (length == 0)
			return array;

		foreach (i; 0 .. length) {
			K k = unpack!K;
			array[k] = unpack!V;
		}

		return array;
	}

	/**
	 * Deserializes the container object and assigns to each argument.
	 *
	 * These methods check the length. Do rollback if
	 * the length of arguments is different from length of deserialized object.
	 *
	 * In unpackMap, the number of arguments must be even.
	 *
	 * Params:
	 *  objects = the references of object to assign.
	 *
	 * Returns: true if succeed
	 */
	bool unpackArray(Types...)(ref Types objects) nothrow if (Types.length > 1) {
		const spos = pos;
		long length = beginArray();
		if (length != Types.length) {
			// the number of deserialized objects is mismatched
			pos = spos;
			return false;
		}

		foreach (i, T; Types)
			try
				objects[i] = unpack!T;
			catch (Exception e) {
				pos = spos;
				return false;
			}
		// unpack(objects);  // slow :(

		return true;
	}

	/// ditto
	bool unpackMap(Types...)(ref Types objects) nothrow if (Types.length > 1) {
		static assert(Types.length % 2 == 0, "The number of arguments must be even");

		const spos = pos;
		long length = beginMap();
		if (length != Types.length >> 1) {
			// the number of deserialized objects is mismatched
			pos = spos;
			return false;
		}

		foreach (i, T; Types)
			try
				objects[i] = unpack!T;
			catch (Exception e) {
				pos = spos;
				return false;
			}

		return this;
	}

	/// ditto
	bool unpackAA(K, V)(K[V] array) nothrow {
		if (unpackNil(array))
			return true;

		long length = beginMap();
		if (length == 0)
			return true;
		if (length < 0)
			return false;

		foreach (i; 0 .. length) {
			try {
				K k = unpack!K;
				array[k] = unpack!V;
			} catch (Exception)
				return false;
		}

		return true;
	}

	long begin(Format f1 = Format.STR, size_t llen = 32, Format f2 = Format.ARRAY16)() nothrow {
		enum Raw = f1 == Format.STR || (Format.BIN8 <= f1 && f1 <= Format.BIN32);
		if (!canRead)
			return -1;
		int header = read();

		if (f1 <= header && header < f1 + llen)
			return header & (llen - 1);
		switch (header) {
			static if (Raw) {
		case Format.BIN8, Format.STR8:
				if (canRead(ubyte.sizeof))
					return read();
				return -1;
		case Format.BIN16, Format.STR16:
			} else {
		case f2:
			}
			if (canRead(ushort.sizeof))
				return read!ushort();
			return -1;
			static if (Raw) {
		case Format.BIN32, Format.STR32:
			} else {
		case cast(Format)(f2 + 1):
			}
			if (canRead(uint.sizeof))
				return read!uint();
			return -1;
		case Format.NIL:
			return 0;
		default:
			pos--;
			return -1;
		}
	}

	/*
	 * Deserializes type-information of raw type.
	 */
	alias beginRaw = begin!();

	/**
	 * Deserializes the type-information of container.
	 *
	 * These methods don't deserialize contents.
	 * You need to call unpack method to deserialize contents at your own risk.
	 *
	 * Returns:
	 *  the container size.
	 */
	alias beginArray = begin!(Format.ARRAY, 16);

	/// ditto
	alias beginMap = begin!(Format.MAP, 16, Format.MAP16);

	version (NoPackingStruct) {
	} else {
		T unpack(T)() if (is(Unqual!T == struct)) {
			T val;
			long len = beginArray();
			version (D_Exceptions) {
				if (len < 0)
					throw new MessagePackException(
						"Attempt to unpack with non-compatible type: expected = array");
			}
			if (len > 0) {
				if (len != NumOfSerializingMembers!T)
					rollback(calculateSize(len), "the number of struct fields is mismatched");
				else
					foreach (i, ref member; val.tupleof)
						static if (isPackedField!(T.tupleof[i]))
							member = unpack!(typeof(member));
			}
			return val;
		}

		bool unpackObj(T)(ref T obj) if (is(Unqual!T == struct)) {
			long len = beginArray();
			if (len == 0)
				return true;
			if (len < 0)
				return false;
			if (len != NumOfSerializingMembers!T) {
				pos -= calculateSize(len) + 1;
				return false; // the number of struct fields is mismatched
			}

			foreach (i, ref member; obj.tupleof)
				static if (isPackedField!(T.tupleof[i]))
					member = unpack!(typeof(member));
			return true;
		}
	}

nothrow:

	/**
	 * Unpacks an EXT value into $(D type) and $(D data).
	 * Returns: true if succeed
	 */
	bool unpackExt(T)(ref byte type, ref T data) if (isOutputBuffer!(T, ubyte)) {
		if (!canRead)
			return false;

		const spos = pos;
		int header = read();
		uint len = void;
		if (header >= Format.EXT && header <= Format.EXT + 4) {
			// Fixed
			len = 1 << (header - Format.EXT);
		} else // Dynamic length
			switch (header) {
		case Format.EXT8:
			if (!canRead(1 + 1)) {
				pos--;
				return false;
			}
			len = read();
			break;
		case Format.EXT16:
			if (!canRead(2 + 1)) {
				pos--;
				return false;
			}
			len = read!ushort();
			break;
		case Format.EXT32:
			if (!canRead(4 + 1)) {
				pos--;
				return false;
			}
			len = read!uint();
			break;
		default:
			pos--;
			return false;
		}

		if (!canRead(len + 1)) {
			pos = spos;
			return false;
		}

		// Read type
		type = read();
		// Read and check data
		AOutputBuf!T(data) ~= read(len);
		return true;
	}

	ubyte peek() => buf[pos];

	/*
	 * Reads value from buffer and advances offset.
	 */
	ubyte read() => buf[pos++];

	auto read(size_t size) {
		auto result = buf[pos .. pos + size];
		pos += size;
		return result;
	}

	/*
	 * Reads $(D_PARAM T) type value from $(D_PARAM buffer).
	 *
	 * Returns:
	 *  the Endian-converted value.
	 */
	T read(T)() {
		auto result = toBE(*cast(const T*)&buf[pos]);
		pos += T.sizeof;
		return result;
	}

	/*
	 * Reading test.
	 *
	 * Params:
	 *  size   = the size to read.
	 */
	bool canRead(size_t size = 1) const {
		static if (__traits(compiles, buf.length))
			return pos + size <= buf.length;
		else
			return true;
	}

	/*
	 * Next object is nil?
	 *
	 * Returns:
	 *  true if next object is nil.
	 */
	bool checkNil() => canRead && peek() == Format.NIL;

	/*
	 * Deserializes nil object and assigns to $(D_PARAM value).
	 *
	 * Params:
	 *  value = the reference of value to assign.
	 *
	 * Returns:
	 *  true if next object is nil.
	 */
	bool unpackNil(T)(ref T value) {
		if (!canRead)
			return false;

		if (peek() == Format.NIL) {
			value = null;
			pos++;
			return true;
		}
		return false;
	}
}

version (unittest) import lmpl4d.packer;

unittest {
	{ // unique
		mixin DefinePacker;

		auto test = tuple(true, false);

		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // uint *
		mixin DefinePacker;

		auto test = tuple(ubyte.max, ushort.max, uint.max, ulong.max);
		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // int *
		mixin DefinePacker;

		auto test = tuple(byte.min, short.min, int.min, -1, -32, long.min);

		packer.pack(test);

		mixin TestUnpacker;
	}
}

unittest {
	{ // floating point
		mixin DefinePacker;

		static if (real.sizeof == double.sizeof || !EnableReal) {
			alias R = double;
		} else {
			alias R = real;
		}
		Tuple!(float, double, R) test = tuple(float.min_normal, double.max, cast(real)R.min_normal);

		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // enum
		enum : float {
			D = 0.5
		}
		enum E : ulong {
			U = 100
		}

		mixin DefinePacker;

		float f = D, resultF;
		E e = E.U, resultE;

		packer.pack(D, e);

		mixin TestUnpacker;

		unpacker.unpack(resultF, resultE);
		assert(f == resultF);
		assert(e == resultE);
	}
}

version (NoPackingStruct) {
} else version (D_Exceptions)
	unittest {
	import std.container.array;

	struct Test {
		string f1;
		@nonPacked int f2;
	}

	mixin DefinePacker;

	Test s = Test("foo", 10), r;

	auto buf = packer.pack(s).buf[];
	auto unpacker = Unpacker!()(buf);
	r = unpacker.unpack!Test;
	assert(s.f1 == r.f1);
	assert(s.f2 != r.f2);
	assert(r.f2 == int.init);

	auto arr2 = Array!ubyte();
	auto packer2 = Packer!(Array!ubyte)(arr2);
	assert(packer2.pack(Test.init).buf.length < buf.length);
}

version (D_Exceptions) unittest {
	import std.conv : text;

	{ // container
		mixin DefinePacker;

		Tuple!(ulong[], int[uint], string, bool[2], char[2], ubyte[32]) test =
			tuple([1UL, 2], [3U: 4, 5: 6, 7: 8], "MessagePack", [true, false], "D!",
				(ubyte[32]).init);

		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // ext

		// Try a variety of lengths, making sure to hit all the fixexts
		foreach (L; AliasSeq!(1, 2, 3, 4, 5, 8, 9, 16, 32, 512, 2 ^^ 16)) {
			mixin DefinePacker;

			auto data = new ubyte[L];
			data.fillData;
			packer.packExt(7, data);

			auto unpacker = Unpacker!()(packer[]);
			byte type;
			ubyte[] deserializedData;

			assert(unpacker.unpackExt(type, deserializedData));
			assert(type == 7, text("type: ", type));
			assert(data == deserializedData, text(data, "\nExpected: ", deserializedData));
		}
	}
}
