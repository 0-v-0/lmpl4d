module lmpl4d.unpacker;

import lmpl4d.common;

struct Unpacker(Stream = ubyte[]) if(isInputBuffer!(Stream, ubyte))
{
	Stream buf;
	size_t pos;
	this(Stream stream) { buf = stream; };

	alias TThis = typeof(this);

	version(betterC){
		void rollback(size_t size, string expected, Format actual = Format.NONE) {
			pos -= size + 1;
		}

		void check(size_t size = 1) {}
	} else {
		T unpack(T)()
		if (is(Unqual!T == enum) || isPointer!T || isSomeChar!T || isNumeric!T || is(Unqual!T == bool))
		{
			static if (is(Unqual!T == enum))
				return cast(T)unpack!(OriginalType!T);
			else static if (isPointer!T) {
				T val;
				if (unpackNil(val))
					return val;
				throw new UnpackException("Can't deserialize a pointer that is not null");
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
							auto val = load!ushort(read(ushort.sizeof));
							if (val > T.max)
								rollback(ushort.sizeof, T.stringof, Format.UINT16);
							return cast(T)val;
						case Format.UINT32:
							check(uint.sizeof);
							auto val = load!uint(read(uint.sizeof));
							if (val > T.max)
								rollback(uint.sizeof, T.stringof, Format.UINT32);
							return cast(T)val;
						case Format.UINT64:
							check(ulong.sizeof);
							auto val = load!ulong(read(ulong.sizeof));
							if (val > T.max)
								rollback(ulong.sizeof, T.stringof, Format.UINT64);
							return cast(T)val;
						case Format.INT8:
							check(byte.sizeof);
							return cast(T)read();
						case Format.INT16:
							check(short.sizeof);
							auto val = load!short(read(short.sizeof));
							if (val < T.min || T.max < val)
								rollback(short.sizeof, T.stringof, Format.INT16);
							return cast(T)val;
						case Format.INT32:
							check(int.sizeof);
							auto val = load!int(read(int.sizeof));
							if (val < T.min || T.max < val)
								rollback(int.sizeof, T.stringof, Format.INT32);
							return cast(T)val;
						case Format.INT64:
							check(long.sizeof);
							auto val = load!long(read(long.sizeof));
							if (val < T.min || T.max < val)
								rollback(long.sizeof, T.stringof, Format.INT64);
							return cast(T)val;
					} else static if (isFloatingPoint!T) {
						case Format.FLOAT:
							_f val;
							check(uint.sizeof);
							val.i = load!uint(read(uint.sizeof));
							return val.f;
						case Format.DOUBLE:
							// check precision loss
							static if (is(Unqual!T == float))
								rollback(0, T.stringof, Format.DOUBLE);

							_d val;
							check(ulong.sizeof);
							val.i = load!ulong(read(ulong.sizeof));
							return val.f;
						case Format.REAL:
							static if (!EnableReal) {
								rollback(0, "real is disabled", Format.REAL);
								break;
							}
							else
							{
								// check precision loss
								static if (is(Unqual!T == float) || is(Unqual!T == double))
									rollback(0, T.stringof, Format.REAL);
								check(real.sizeof);
								version (NonX86)
								{
									CustomFloat!80 tmp;

									const frac = load!ulong (read(ulong.sizeof));
									const exp  = load!ushort(read(ushort.sizeof));

									tmp.significand = frac;
									tmp.exponent    = exp & 0x7fff;
									tmp.sign        = (exp & 0x8000) != 0;

									// NOTE: tmp.get!real is inf on non-x86 when deserialized value is larger than double.max.
									return tmp.get!real;
								}
								else
								{
									_r tmp;

									tmp.fraction = load!(typeof(tmp.fraction))(read(tmp.fraction.sizeof));
									tmp.exponent = load!(typeof(tmp.exponent))(read(tmp.exponent.sizeof));

									return tmp.f;
								}
							}
						}
						default: break;
					}
					rollback(0, T.stringof, cast(Format)header);
				}
			assert(0, "Unsupported type");
		}

		void rollback(size_t size, string expected, Format actual = Format.NONE) {
			import std.conv : text;
			pos -= size + 1;
			throw new MessagePackException(text("Attempt to unpack with non-compatible type: ",
				actual ? text("expected = ", expected, ", got = ", actual) : expected));
		}
		void check(size_t size = 1) {
			if(!canRead(size)) throw new UnpackException("Insufficient buffer");
		}
	}

	T unpack(T)(T defValue) nothrow
	if (is(Unqual!T == enum) || isPointer!T || isTuple!T || isSomeChar!T || isNumeric!T || is(Unqual!T == bool))
	{
		static if (is(Unqual!T == enum))
			return cast(T)unpack(cast(OriginalType!T)defValue);
		else static if (isPointer!T) {
			T val;
			return unpackNil(val) ? val : defValue;
		} else static if (isTuple!T) {
			T val;
			unpackArray!(T.Types)(val.field);
			return val;
		} else if (is(Unqual!T == char))
			return cast(T)unpack(cast(ubyte)defValue);
		else static if (is(Unqual!T == wchar))
			return cast(T)unpack(cast(ushort)defValue);
		else static if (is(Unqual!T == dchar))
			return cast(T)unpack(cast(uint)defValue);
		else static if (isNumeric!T || is(Unqual!T == bool)) {
			if(!canRead) return defValue;
			int header = read();
			static if (isIntegral!T) {
				if (header <= 0x7f)
					return cast(T)header;
			}
			switch (header) {
			static if (is(Unqual!T == bool)) {
				case Format.TRUE:
					return true;
				case Format.FALSE:
					return false;
			} else static if (isIntegral!T) {
				case Format.UINT8:
					if(!canRead(ubyte.sizeof)) return defValue;
					return read();
				case Format.UINT16:
					if(!canRead(ushort.sizeof)) return defValue;
					auto val = load!ushort(read(ushort.sizeof));
					if (val > T.max)
						return defValue;
					return cast(T)val;
				case Format.UINT32:
					if(!canRead(uint.sizeof)) return defValue;
					auto val = load!uint(read(uint.sizeof));
					if (val > T.max)
						return defValue;
					return cast(T)val;
				case Format.UINT64:
					if(!canRead(ulong.sizeof)) return defValue;
					auto val = load!ulong(read(ulong.sizeof));
					if (val > T.max)
						return defValue;
					return cast(T)val;
				case Format.INT8:
					if(!canRead(byte.sizeof)) return defValue;
					return cast(T)read();
				case Format.INT16:
					if(!canRead(short.sizeof)) return defValue;
					auto val = load!short(read(short.sizeof));
					if (val < T.min || T.max < val)
						return defValue;
					return cast(T)val;
				case Format.INT32:
					if(!canRead(int.sizeof)) return defValue;
					auto val = load!int(read(int.sizeof));
					if (val < T.min || T.max < val)
						return defValue;
					return cast(T)val;
				case Format.INT64:
					if(!canRead(long.sizeof)) return defValue;
					auto val = load!long(read(long.sizeof));
					if (val < T.min || T.max < val)
						return defValue;
					return cast(T)val;
			} else static if (isFloatingPoint!T) {
				case Format.FLOAT:
					_f val;
					if(!canRead(uint.sizeof)) return defValue;
					val.i = load!uint(read(uint.sizeof));
					return val.f;
				case Format.DOUBLE:
					// check precision loss
					static if (is(Unqual!T == float))
						return defValue;
					else {
						_d val = void;
						if(!canRead(ulong.sizeof)) return defValue;
						val.i = load!ulong(read(ulong.sizeof));
						return val.f;
					}
				case Format.REAL:
					static if (!EnableReal) return defValue;
					else
					{
						// check precision loss
						static if (is(Unqual!T == float) || is(Unqual!T == double))
							return defValue;
						if(!canRead(real.sizeof)) return defValue;
						version (NonX86)
						{
							CustomFloat!80 tmp;

							const frac = load!ulong (read(ulong.sizeof));
							const exp  = load!ushort(read(ushort.sizeof));

							tmp.significand = frac;
							tmp.exponent    = exp & 0x7fff;
							tmp.sign        = (exp & 0x8000) != 0;

							// NOTE: tmp.get!real is inf on non-x86 when deserialized value is larger than double.max.
							return tmp.get!real;
						}
						else
						{
							_r tmp = void;

							tmp.fraction = load!(typeof(tmp.fraction))(read(tmp.fraction.sizeof));
							tmp.exponent = load!(typeof(tmp.exponent))(read(tmp.exponent.sizeof));

							return tmp.f;
						}
					}
				}
				default: return defValue;
			}
		}
	}

	/// ditto
	ref TThis unpack(Types...)(ref Types objects) if (Types.length > 1)
	{
		foreach (i, T; Types)
			objects[i] = unpack!T;
		return this;
	}

	T unpack(T)() if (isSomeArray!T)
	{
		alias typeof(T.init[0]) U;
		if (checkNil()) {
			static if (isStaticArray!T) {
				pos++;
				rollback(0, "static array", Format.NIL);
			}
			else {
				T array = void;
				unpackNil(array);
				return array;
			}
		}
		enum RawBytes = isByte!U || isSomeChar!U;
		static if (RawBytes)
			auto length = beginRaw();
		else
			auto length = beginArray();
		version(betterC) {} else {
			static if (__traits(compiles, buf.length))
			if (pos + length > buf.length) {
				import std.conv: text;
				throw new MessagePackException(text("Invalid array size in byte stream: Length (", length,
													") is larger than internal buffer size (", buf.length, ")"));
			}
		}
		static if (isStaticArray!T)
			T array = void;
		else {
			import std.array;
			T array = uninitializedArray!T(length);
		}
		if (length == 0)
			return array;
		static if (RawBytes) {
			auto offset = calculateSize!(true)(length);
			check(length + offset);
			static if (isStaticArray!T)
				array = (cast(U[])read(length))[0 .. T.length];
			else
				array = cast(T)read(length);
		} else
			foreach (ref a; array)
				a = unpack!U;
		return array;
	}

	bool unpack(T)(ref T array) nothrow if (isSomeArray!T)
	{
		import std.array;

		alias typeof(T.init[0]) U;
		const spos = pos;
		if (checkNil()) {
			static if (isStaticArray!T)
				return false;
			else
				return unpackNil(array);
		}
		if (!canRead)
			return false;
		enum RawBytes = isByte!U || isSomeChar!U;
		static if (RawBytes)
			auto length = beginRaw();
		else
			auto length = beginArray();
		version(betterC) {} else {
			static if (__traits(compiles, buf.length))
				if (length > buf.length) {
					pos = spos;
					return false;
				}
		}
		if (length == 0)
			return true;
		static if (!isStaticArray!T)
			if (array.length != length)
				array = uninitializedArray!T(length);
		static if (RawBytes) {
			auto offset = calculateSize!(true)(length);
			if(!canRead(length + offset)) {
				pos = spos;
				return false;
			}
			static if (isStaticArray!T)
				array = (cast(U[])read(length))[0 .. T.length];
			else
				array = cast(T)read(length);
		} else
			foreach (ref a; array)
				a = unpack!U;
		return true;
	}

	/// ditto
	T unpack(T)() if (isAssociativeArray!T)
	{
		alias typeof(T.init.keys[0])   K;
		alias typeof(T.init.values[0]) V;
		T array;

		if (unpackNil(array))
			return array;

		auto length = beginMap();
		if (length == 0)
			return array;

		foreach (i; 0..length) {
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
	bool unpackArray(Types...)(ref Types objects) nothrow if (Types.length > 1)
	{
		auto length = beginArray();
		const spos = pos;
		if (length != Types.length) {
			//the number of deserialized objects is mismatched
			pos = spos;
			return false;
		}

		foreach (i, T; Types)
			try {
				objects[i] = unpack!T;
			} catch (Exception e) {
				pos = spos;
				return false;
			}
		// unpack(objects);  // slow :(

		return true;
	}

	/// ditto
	bool unpackMap(Types...)(ref Types objects) nothrow if (Types.length > 1)
	{
		static assert(Types.length % 2 == 0, "The number of arguments must be even");

		auto length = beginMap();
		const spos = pos;
		if (length != Types.length >> 1) {
			// the number of deserialized objects is mismatched
			pos = spos;
			return false;
		}

		foreach (i, T; Types)
			try {
				objects[i] = unpack!T;
			} catch (Exception e) {
				pos = spos;
				return false;
			}

		return this;
	}

	/// ditto
	bool unpackAA(K, V)(K[V] array) nothrow
	{
		if (unpackNil(array))
			return true;

		auto length = beginMap();
		if (length == 0)
			return true;

		foreach (i; 0..length) {
			try {
				K k = unpack!K;
				array[k] = unpack!V;
			} catch (Exception e) {
				return false;
			}
		}

		return true;
	}

	size_t begin(int s1 = 0xa0, int s2 = 0xbf, Format f = Format.ARRAY16)() nothrow
	{
		enum Raw = s1 == 0xa0 && s2 == 0xbf;
		if(!canRead) return 0;
		int header = read();

		if (s1 <= header && header <= s2)
			return header & (s2 - s1);
		switch (header) {
			static if(Raw) {
				case Format.BIN8, Format.STR8:
				if(!canRead(ubyte.sizeof)) return 0;
				return read();
				case Format.BIN16, Format.RAW16:
			} else {
				case f:
			}
			if(!canRead(ushort.sizeof)) return 0;
			return load!ushort(read(ushort.sizeof));
			static if(Raw) {
				case Format.BIN32, Format.RAW32:
			} else {
				case cast(Format)(f + 1):
			}
			if(!canRead(uint.sizeof)) return 0;
			return load!uint(read(uint.sizeof));
		case Format.NIL:
			return 0;
		default:
			pos--;
			import std.conv: text;
			assert(0, text("Attempt to unpack with non-compatible type: expected = ",
				f.stringof, ", got = ", header));
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
	alias beginArray = begin!(0x90, 0x9f);

	/// ditto
	alias beginMap = begin!(0x80, 0x8f, Format.MAP16);

	version(NoPackingStruct) {}
	else {
		T unpack(T)() if (is(Unqual!T == struct)) {
			T val;
			size_t len = beginArray();
			if (len) {
				if (len != NumOfSerializingMembers!T)
					rollback(calculateSize(len), "the number of struct fields is mismatched");

				foreach (i, ref member; val.tupleof)
					static if (isPackedField!(T.tupleof[i]))
						member = unpack!(typeof(member));
			}
			return val;
		}

		bool unpackObj(T)(ref T obj) if (is(Unqual!T == struct)) {
			size_t len = beginArray();
			if (len == 0)
				return true;
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
	bool unpackExt(T)(ref byte type, ref T data) if(isOutputBuffer!(T, ubyte))
	{
		if(!canRead) return false;
		int header = read();
		import std.conv : text;

		uint len = void;
		uint rollbackLen = 1;
		if (header >= Format.EXT && header <= Format.EXT + 4) {
			// Fixed
			len = 1 << (header - Format.EXT);
		} else
			// Dynamic length
			switch (header)
			{
				case Format.EXT8:
					if(!canRead(1 + 1)) {
						pos--;
						return false;
					}
					len = read();
					rollbackLen++;
					break;
				case Format.EXT16:
					if(!canRead(2 + 1)) {
						pos--;
						return false;
					}
					len = load!ushort(read(2));
					rollbackLen += 2;
					break;
				case Format.EXT32:
					if(!canRead(4 + 1)) {
						pos--;
						return false;
					}
					len = load!uint(read(4));
					rollbackLen += 4;
					break;
				default:
					pos--;
					return false;
			}

		if(!canRead(len + 1)) {
			pos -= rollbackLen;
			return false;
		}

		// Read type
		type = read();
        // Read and check data
		auto obuf = AOutputBuf!T(data);
		obuf ~= read(len);
		return true;
	}

	/*
	 * Reads value from buffer and advances offset.
	 */
	ubyte read()
	{
		return buf[pos++];
	}

	ubyte[] read(size_t size)
	{
		auto result = buf[pos..pos+size];
		pos += size;
		return result;
	}

	/*
	 * Reading test.
	 *
	 * Params:
	 *  size   = the size to read.
	 */
	bool canRead(size_t size = 1) const
	{
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
	bool checkNil()
	{
		return canRead && buf[pos] == Format.NIL;
	}

	/*
	 * Deserializes nil object and assigns to $(D_PARAM value).
	 *
	 * Params:
	 *  value = the reference of value to assign.
	 *
	 * Returns:
	 *  true if next object is nil.
	 */
	bool unpackNil(T)(ref T value)
	{
		if(!canRead)
			return false;

		if (buf[pos] == Format.NIL) {
			value = null;
			pos++;
			return true;
		}
		return false;
	}

	/*
	 * Loads $(D_PARAM T) type value from $(D_PARAM buffer).
	 *
	 * Params:
	 *  buffer = the serialized contents.
	 *
	 * Returns:
	 *  the Endian-converted value.
	 */
	package T load(T)(ubyte[] buf)
	{
		static if (isIntegral!T && T.sizeof == 2)
			enum bit = 16;
		else static if (isIntegral!T && T.sizeof == 4)
			enum bit = 32;
		else static if (isIntegral!T && T.sizeof == 8)
			enum bit = 64;
		else static assert(0, "Unsupported type");
		return convertEndianTo!bit(*cast(T*)buf.ptr);
	}
}

version(unittest)
	import
		lmpl4d.packer,
		std.exception;

unittest
{
	{ // unique
		mixin DefinePacker;

		Tuple!(bool, bool) test = tuple(true, false), result;

		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // uint *
		mixin DefinePacker;

		Tuple!(ubyte, ushort, uint, ulong) test = tuple(ubyte.max, ushort.max, uint.max, ulong.max),
										result;
		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // int *
		mixin DefinePacker;

		Tuple!(byte, short, int, long) test = tuple(byte.min, short.min,
													int.min,   long.min),
										result;

		packer.pack(test);

		mixin TestUnpacker;
	}
}
unittest
{
	{ // floating point
		mixin DefinePacker;

		static if (real.sizeof == double.sizeof || !EnableReal)
		{
			alias R = double;
		}
		else
		{
			alias R = real;
		}
		Tuple!(float, double, R) test = tuple(float.min_normal, double.max, cast(real)R.min_normal),
								result;

		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // enum
		enum   : float { D = 0.5 }
		enum E : ulong { U = 100 }

		mixin DefinePacker;

		float f = D,   resultF;
		E     e = E.U, resultE;

		packer.pack(D, e);

		mixin TestUnpacker;

		unpacker.unpack(resultF, resultE);
		assert(f == resultF);
		assert(e == resultE);
	}
}

version(NoPackingStruct) {}
else unittest
{
	struct Test
	{
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

unittest
{
	import std.conv : text;
	{ // container
		mixin DefinePacker;

		Tuple!(ulong[], int[uint], string, bool[2], char[2]) test =
			tuple([1UL, 2], [3U:4, 5:6, 7:8], "MessagePack", [true, false], "D!");

		packer.pack(test);

		mixin TestUnpacker;
	}
	{ // ext

		// Try a variety of lengths, making sure to hit all the fixexts
		foreach (L; AliasSeq!(1, 2, 3, 4, 5, 8, 9, 16, 32, 512, 2^^16))
		{
			mixin DefinePacker;

			auto data = new ubyte[L];
			data.fillData;
			packer.packExt(7, data);

			auto unpacker = Unpacker!()(packer.buf[]);
			byte type;
			ubyte[] deserializedData;

			assert(unpacker.unpackExt(type, deserializedData));
			assert(type == 7, text("type: ", type));
			assert(data == deserializedData, text(data, "\nExpected: ", deserializedData));
		}
	}
}