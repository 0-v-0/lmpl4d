module lmpl4d.unpacker;

import lmpl4d.common;

enum MsgPackErr {
	ok,
	insufficientBuffer,
	invalidType,
	arraySizeMismatch,
	mapSizeMismatch,
}

struct Unpacker(Stream = const(ubyte)[]) if (isInputBuffer!(Stream, ubyte)) {
	Stream buf;
	size_t pos;
	this(Stream stream) {
		buf = stream;
	}

	version (D_Exceptions) {
		import std.conv : text;

		void rollback(size_t size, string expected, Format actual = Format.NONE) {
			pos -= size + 1;
			throw new Ex(text("Attempt to unpack with non-compatible type: ",
					actual ? text("expected = ", expected, ", got = ", actual) : expected));
		}

		void check(size_t size = 1) {
			if (!canRead(size))
				throw new Ex(text("Insufficient buffer: ", size, " bytes required"));
		}
	} else {
		void rollback(size_t size, string expected, Format actual = Format.NONE) {
			pos -= size + 1;
		}

		void check(size_t size = 1) {
		}
	}

	T unpack(T)() if (is(T == enum) || isPointer!T || isScalarType!T) {
		static if (is(T == enum))
			return cast(T)unpack!(OriginalType!T);
		else static if (isPointer!T) {
			T val;
			version (D_Exceptions) {
				if (!unpackNil(val))
					throw new Ex("Can't deserialize a pointer that is not null");
			}
			return val;
		} else static if (isSomeChar!T)
			return cast(T)unpack!(AliasSeq!(ubyte, ushort, uint)[T.sizeof / 2]);
		else static if (isScalarType!T) {
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
						check(ulong.sizeof + ushort.sizeof);
						const frac = read!ulong();
						const exp = read!ushort();
						return getReal(frac, exp);
					} else {
						rollback(0, "real is disabled", Format.REAL);
						break;
					}
				}
			default:
			}
			rollback(0, T.stringof, cast(Format)header);
		}
		assert(0, "Unsupported type");
	}

	T unpack(T)(ref T defValue) nothrow
	if (is(T == enum) || isTuple!T) {
		static if (is(T == enum)) {
			alias O = OriginalType!T;
			static if (isSomeArray!O) {
				() @trusted { unpack(*cast(O*)&defValue); }();
				return defValue;
			} else
				return cast(T)unpack(cast(O)defValue);
		} else static if (isTuple!T) {
			T val;
			unpackArray!(T.Types)(val.field);
			return val;
		}
	}

	T unpack(T)(T defValue) nothrow
	if ((isPointer!T || isScalarType!T) && !is(T == enum)) {
		static if (isPointer!T) {
			T val;
			return unpackNil(val) == MsgPackErr.ok ? val : defValue;
		} else static if (isSomeChar!T)
			return cast(T)unpack(cast(AliasSeq!(ubyte, ushort, uint)[T.sizeof / 2])defValue);
		else static if (isScalarType!T) {
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
						if (!canRead(ulong.sizeof + ushort.sizeof))
							return defValue;
						const frac = read!ulong();
						const exp = read!ushort();
						return getReal(frac, exp);
					} else
						return defValue;
				}
			default:
				pos = spos;
				return defValue;
			}
		}
	}

	ref unpackTo(Types...)(ref Types objects) {
		foreach (i, T; Types)
			objects[i] = unpack!T;
		return this;
	}

	/// ditto
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
		static if (U.sizeof == 1)
			const len = beginBin();
		else
			const len = beginArray();
		version (D_Exceptions) {
			import std.conv : text;

			if (len < 0)
				throw new Ex(
					"Attempt to unpack with non-compatible type or buffer is insufficient: expected = array");
			static if (isStaticArray!T)
				if (len != T.length)
					throw new Ex(text("Static array length mismatch: got = ", len,
							"expected = ", T.length));
			static if (__traits(compiles, buf.length))
				if (pos + len > buf.length)
					throw new Ex(text("Invalid array size in byte stream: Length (", len,
							") is larger than internal buffer size (", buf.length, ")"));
		} else {
			if (len < 0)
				return T.init;
			static if (isStaticArray!T) {
				if (len != T.length)
					return T.init;
			}
		}
		static if (isStaticArray!T)
			T array = void;
		else version (D_BetterC)
				static assert(0, "Dynamic array unpacking is not supported in BetterC mode");
			else {
				import std.array;

				T array = uninitializedArray!T(len);
			}
		if (len == 0)
			return array;
		static if (U.sizeof == 1) {
			check(len);
			static if (isStaticArray!T)
				array = (cast(U[])read(len))[0 .. T.length];
			else
				array = cast(T)read(len);
		} else
			foreach (ref a; array)
				a = unpack!U;
		return array;
	}

	MsgPackErr unpack(T)(ref T array) nothrow if (isSomeArray!T) {
		alias U = typeof(T.init[0]);

		const spos = pos;
		if (checkNil()) {
			static if (isStaticArray!T)
				return MsgPackErr.invalidType;
			else
				return unpackNil(array);
		}
		static if (U.sizeof == 1)
			const len = beginBin();
		else
			const len = beginArray();
		if (len < 0)
			return cast(MsgPackErr)-len;
		static if (__traits(compiles, buf.length))
			if (len > buf.length) {
				pos = spos;
				return MsgPackErr.insufficientBuffer;
			}
		static if (isStaticArray!T)
			if (len != T.length) {
				pos = spos;
				return MsgPackErr.arraySizeMismatch;
			}
		if (len == 0)
			return MsgPackErr.ok;
		static if (U.sizeof == 1) {
			if (!canRead(len)) {
				pos = spos;
				return MsgPackErr.insufficientBuffer;
			}
			static if (isStaticArray!T)
				array = (cast(U[])read(len))[0 .. T.length];
			else
				array = cast(T)read(len);
		} else {
			static if (!isStaticArray!T) {
				if (array.length != len) {
					version (D_BetterC) {
						pos = spos;
						return MsgPackErr.arraySizeMismatch;
					} else {
						import std.array;

						array = uninitializedArray!T(len);
					}
				}
			}
			try {
				foreach (ref a; array)
					a = unpack!U;
			} catch (Exception)
				return MsgPackErr.invalidType;
		}
		return MsgPackErr.ok;
	}

	/// ditto
	T unpack(T : V[K], K, V)() {
		T array;

		if (unpackNil(array) == MsgPackErr.ok)
			return array;

		const len = beginMap();
		if (len < 0)
			throw new Ex("Attempt to unpack with non-compatible type: expected = map");

		foreach (i; 0 .. len) {
			K k = unpack!K;
			array[k] = unpack!V;
		}

		return array;
	}

	/++
		Deserializes the container object and assigns to each argument.

		These methods check the length. Do rollback if
		the length of arguments is different from length of deserialized object.

		In unpackMap, the number of arguments must be even.

	Params:
		objects = the references of object to assign.

	Returns: true if succeed
	+/
	MsgPackErr unpackArray(Types...)(ref Types objects) nothrow if (Types.length > 1) {
		const spos = pos;
		const len = beginArray();
		if (len != Types.length) {
			// the number of deserialized objects is mismatched
			pos = spos;
			return len < 0 ? cast(MsgPackErr)-len : MsgPackErr.arraySizeMismatch;
		}

		foreach (i, T; Types)
			try
				objects[i] = unpack!T;
			catch (Exception) {
				pos = spos;
				return MsgPackErr.invalidType;
			}
		// unpack(objects);  // slow :(

		return MsgPackErr.ok;
	}

	/// ditto
	MsgPackErr unpackMap(Types...)(ref Types objects) nothrow if (Types.length > 1) {
		static assert(Types.length % 2 == 0, "The number of arguments must be even");

		const spos = pos;
		const len = beginMap();
		if (len != Types.length >> 1) {
			// the number of deserialized objects is mismatched
			pos = spos;
			return len < 0 ? cast(MsgPackErr)-len : MsgPackErr.mapSizeMismatch;
		}

		foreach (i, T; Types)
			try
				objects[i] = unpack!T;
			catch (Exception) {
				pos = spos;
				return MsgPackErr.invalidType;
			}

		return MsgPackErr.ok;
	}

	/// ditto
	MsgPackErr unpackAA(K, V)(K[V] array) nothrow {
		if (unpackNil(array) == MsgPackErr.ok)
			return MsgPackErr.ok;

		const len = beginMap();
		if (len == 0)
			return MsgPackErr.ok;
		if (len < 0)
			return cast(MsgPackErr)-len;

		foreach (i; 0 .. len) {
			try {
				K k = unpack!K;
				array[k] = unpack!V;
			} catch (Exception)
				return MsgPackErr.invalidType;
		}

		return MsgPackErr.ok;
	}

	long begin(Format f1 = Format.STR, size_t llen = 32, Format f2 = Format.ARRAY16)() nothrow {
		enum Raw = f1 == Format.STR || (Format.BIN8 <= f1 && f1 <= Format.BIN32);
		if (!canRead)
			return -MsgPackErr.insufficientBuffer;
		const int header = read();

		if (f1 <= header && header < f1 + llen)
			return header & (llen - 1);
		switch (header) {
			static if (Raw) {
		case Format.BIN8, Format.STR8:
				if (canRead(ubyte.sizeof))
					return read();
				return -MsgPackErr.insufficientBuffer;
		case Format.BIN16, Format.STR16:
			} else {
		case f2:
			}
			if (canRead(ushort.sizeof))
				return read!ushort();
			return -MsgPackErr.insufficientBuffer;
			static if (Raw) {
		case Format.BIN32, Format.STR32:
			} else {
		case cast(Format)(f2 + 1):
			}
			if (canRead(uint.sizeof))
				return read!uint();
			return -MsgPackErr.insufficientBuffer;
		case Format.NIL:
			return 0;
		default:
			pos--;
			return -MsgPackErr.invalidType;
		}
	}

	/++
		Deserializes type-information of raw type.
	+/
	alias beginBin = begin!();

	/++
	Deserializes the type-information of container.

	These methods don't deserialize contents.
	You need to call unpack method to deserialize contents at your own risk.

	Returns: the container size.
	+/
	alias beginArray = begin!(Format.ARRAY, 16);

	/// ditto
	alias beginMap = begin!(Format.MAP, 16, Format.MAP16);

	version (NoPackingStruct) {
	} else {
		T unpack(T)() if (is(T == struct)) {
			T val;
			const len = beginArray();
			version (D_Exceptions) {
				if (len < 0)
					throw new Ex(
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

		MsgPackErr unpackObj(T)(ref T obj) if (is(T == struct)) {
			const len = beginArray();
			if (len == 0)
				return MsgPackErr.ok;
			if (len < 0)
				return cast(MsgPackErr)-len;
			if (len != NumOfSerializingMembers!T) {
				// the number of struct fields is mismatched
				pos -= calculateSize(len) + 1;
				return MsgPackErr.arraySizeMismatch;
			}

			foreach (i, ref member; obj.tupleof)
				static if (isPackedField!(T.tupleof[i]))
					member = unpack!(typeof(member));
			return MsgPackErr.ok;
		}
	}

nothrow:

	/++
		Unpacks an EXT value into `type` and `data`.
		Returns: true if succeed
	+/
	MsgPackErr unpackExt(T)(ref byte type, ref T data)
	if (isOutputBuffer!(T, ubyte)) {
		if (!canRead)
			return MsgPackErr.insufficientBuffer;

		const spos = pos;
		const int header = read();
		uint len = void;
		if (header >= Format.EXT && header <= Format.EXT + 4) {
			// Fixed
			len = 1 << (header - Format.EXT);
		} else // Dynamic length
			switch (header) {
		case Format.EXT8:
			if (!canRead(1 + 1)) {
				pos--;
				return MsgPackErr.insufficientBuffer;
			}
			len = read();
			break;
		case Format.EXT16:
			if (!canRead(2 + 1)) {
				pos--;
				return MsgPackErr.insufficientBuffer;
			}
			len = read!ushort();
			break;
		case Format.EXT32:
			if (!canRead(4 + 1)) {
				pos--;
				return MsgPackErr.insufficientBuffer;
			}
			len = read!uint();
			break;
		default:
			pos--;
			return MsgPackErr.invalidType;
		}

		if (!canRead(len + 1)) {
			pos = spos;
			return MsgPackErr.insufficientBuffer;
		}

		// Read type
		type = read();
		// Read and check data
		AOutputBuf!T(data) ~= read(len);
		return MsgPackErr.ok;
	}

	ubyte peek() => buf[pos];

	/++
		Reads value from buffer and advances offset.
	+/
	ubyte read() => buf[pos++];

	auto read(size_t size) {
		auto result = buf[pos .. pos + size];
		pos += size;
		return result;
	}

	/++
		Reads `T` type value from `buffer`.

	Returns: the Endian-converted value.
	+/
	T read(T)() {
		auto result = toBE(*cast(const T*)&buf[pos]);
		pos += T.sizeof;
		return result;
	}

	/++
		Reading test.

	Params:
		size = the size to read.
	+/
	bool canRead(size_t size = 1) const {
		static if (__traits(compiles, buf.length))
			return pos + size <= buf.length;
		else
			return true;
	}

	/++
	Next object is nil?

	Returns: true if next object is nil.
	+/
	bool checkNil() => canRead && peek() == Format.NIL;

	/++
		Deserializes nil object and assigns to `value`.

	Params:
		value = the reference of value to assign.

	Returns: true if next object is nil.
	+/
	MsgPackErr unpackNil(T)(ref T value) {
		if (!canRead)
			return MsgPackErr.insufficientBuffer;

		if (peek() == Format.NIL) {
			value = null;
			pos++;
			return MsgPackErr.ok;
		}
		return MsgPackErr.invalidType;
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

		static if (real.sizeof == double.sizeof || !EnableReal)
			alias R = double;
		else
			alias R = real;
		auto test = tuple(float.min_normal, double.max, R.min_normal);

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

		unpacker.unpackTo(resultF, resultE);
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

			assert(unpacker.unpackExt(type, deserializedData) == MsgPackErr.ok);
			assert(type == 7, text("type: ", type));
			assert(data == deserializedData, text(data, "\nExpected: ", deserializedData));
		}
	}
}
