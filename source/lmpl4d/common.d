module lmpl4d.common;

import
	core.bitop,
	std.container.array,
	std.meta;

package import
	std.traits,
	std.typecons;

version(EnableReal)
	enum EnableReal = true;
else
	enum EnableReal = false;

version(NoPackingStruct) {}
else {
	struct nonPacked {}

	package enum isPackedField(alias field) = staticIndexOf!(nonPacked, __traits(getAttributes, field)) == -1
		&& !isSomeFunction!(typeof(field));

	/**
	 * Get the number of member to serialize.
	 */
	template NumOfSerializingMembers(Classes...)
	{
		static if (Classes.length)
			enum NumOfSerializingMembers = Filter!(isPackedField, Classes[0].tupleof).length +
				NumOfSerializingMembers!(Classes[1..$]);
		else
			enum NumOfSerializingMembers = 0;
	}
}

package:

enum isSomeArray(T) = (isArray!T || isInstanceOf!(Array, T)) && !is(Unqual!T == enum);

static if (real.sizeof == double.sizeof) {
	// for 80bit real inter-operation on non-x86 CPU
	version = NonX86;

	import std.numeric;
}

/**
 * For float type (de)serialization
 */
union _f { float f; uint i; }


/**
 * For double type (de)serialization
 */
union _d { double f; ulong i; }


/**
 * For real type (de)serialization
 *
 * 80-bit real is padded to 12 bytes(Linux) and 16 bytes(Mac).
 * http://lists.puremagic.com/pipermail/digitalmars-d/2010-June/077394.html
 */
union _r
{
	real f;

	struct
	{
		ulong  fraction;
		ushort exponent;  // includes sign
	}
}

/**
 * Detects whether $(D_PARAM T) is a built-in byte type.
 */
enum isByte(T) = staticIndexOf!(Unqual!T, byte, ubyte) >= 0;

/**
 * Gets asterisk string from pointer type
 */
template AsteriskOf(T)
{
	static if (is(T P == U*, U))
		enum AsteriskOf = "*" ~ AsteriskOf!U;
	else
		enum AsteriskOf = "";
}

public:

version (LittleEndian)
{
	/*
	 * Converts $(value) to different Endian.
	 *
	 * Params:
	 *  value = the LittleEndian value to convert.
	 *
	 * Returns:
	 *  the converted value.
	 */
	@trusted
	ushort convertEndianTo(size_t Bit, T)(in T value) if (Bit == 16)
	{
		return byteswap(cast(ushort)value);
	}


	// ditto
	@trusted
	uint convertEndianTo(size_t Bit, T)(in T value) if (Bit == 32)
	{
		return bswap(cast(uint)value);
	}

	// ditto
	@trusted
	ulong convertEndianTo(size_t Bit, T)(in T value) if (Bit == 64)
	{
		return bswap(value);
	}

	unittest
	{
		assert(convertEndianTo!16(0x0123)             == 0x2301);
		assert(convertEndianTo!32(0x01234567)         == 0x67452301);
		assert(convertEndianTo!64(0x0123456789abcdef) == 0xefcdab8967452301);
	}

	/*
	 * Comapatible for BigEndian environment.
	 */
	ubyte take8from(size_t bit = 8, T)(T value)
	if (bit == 8 || bit == 16 || bit == 32 || bit == 64)
	{
		return (cast(ubyte*)&value)[0];
	}

	unittest
	{
		foreach (Integer; AliasSeq!(ubyte, ushort, uint, ulong)) {
			assert(take8from!8 (cast(Integer)0x01)               == 0x01);
			assert(take8from!16(cast(Integer)0x0123)             == 0x23);
			assert(take8from!32(cast(Integer)0x01234567)         == 0x67);
			assert(take8from!64(cast(Integer)0x0123456789abcdef) == 0xef);
		}
	}
}
else
{
	/*
	 * Comapatible for LittleEndian environment.
	 */
	@safe
	ushort convertEndianTo(size_t bit, T)(in T value)
	if (bit == 16 || bit == 32 || bit == 64)
	{
		static if (bit == 16)
			alias U = ushort;
		else static if (bit == 32)
			alias U = uint;
		else
			alias U = ulong;
		return cast(U)value;
	}

	unittest
	{
		assert(convertEndianTo!16(0x0123)       == 0x0123);
		assert(convertEndianTo!32(0x01234567)   == 0x01234567);
		assert(convertEndianTo!64(0x0123456789) == 0x0123456789);
	}


	/*
	 * Takes 8bit from $(D_PARAM value)
	 *
	 * Params:
	 *  value = the content to take.
	 *
	 * Returns:
	 *  the 8bit value corresponding $(D_PARAM bit) width.
	 */
	ubyte take8from(size_t bit = 8, T)(T value)
	if (bit == 8 || bit == 16 || bit == 32 || bit == 64)
	{
		return (cast(ubyte*)&value)[(bit >> 3) - 1];
	}

	unittest
	{
		foreach (Integer; AliasSeq!(ubyte, ushort, uint, ulong)) {
			assert(take8from!8 (cast(Integer)0x01)               == 0x01);
			assert(take8from!16(cast(Integer)0x0123)             == 0x23);
			assert(take8from!32(cast(Integer)0x01234567)         == 0x67);
			assert(take8from!64(cast(Integer)0x0123456789abcdef) == 0xef);
		}
	}
}

enum isInputBuffer(R, E) = __traits(compiles, (R r, size_t i) { E e = r[i++]; });
enum isOutputBuffer(R, E) =
	__traits(compiles, (R r, E e) { r.reserve(1); r ~= e; r = R(&r[0], 1, 1); }) || __traits(compiles,
	(R r, E e) { size_t sz = r.length; r.length = sz + E.sizeof; *cast(Unqual!E*)&r[sz] = e; })
	|| __traits(compiles, (R r, E e) { *cast(Unqual!E*)&r[0] = e; r += E.sizeof; });

unittest
{
	static assert(!isInputBuffer!(void[], ubyte));
	static assert(isInputBuffer!(ubyte[9], ubyte));
	static assert(isInputBuffer!(ubyte[], ubyte));
	static assert(isInputBuffer!(ubyte*, ubyte));
	static assert(isInputBuffer!(Array!ubyte, ubyte));
	static assert(!isInputBuffer!(ubyte, ubyte));
	static assert(!isInputBuffer!(int[], ubyte));
	static assert(isOutputBuffer!(void[], ubyte));
	static assert(!isOutputBuffer!(ubyte[9], ubyte));
	static assert(isOutputBuffer!(ubyte[], ubyte));
	static assert(isOutputBuffer!(ubyte*, ubyte));
	static assert(isOutputBuffer!(Array!ubyte, ubyte));
	static assert(isOutputBuffer!(int[], ubyte));
}

version(betterC){}else {
	@trusted:
	/**
	 * $(D MessagePackException) is a root Exception for MessagePack related operation.
	 */
	class MessagePackException : Exception
	{
		this(string msg) { super(msg); }
	}
	/**
	 * $(D UnpackException) is thrown on deserialization failure
	 */
	class UnpackException : MessagePackException
	{
		this(string msg) { super(msg); }
	}
}

version(unittest)
{
	package void fillData(T)(ref T data)
	{
		import core.stdc.stdlib;
		import std.datetime.systime;

		srand(cast(uint)Clock.currStdTime);
		foreach(ref x; data)
			x = cast(ubyte)rand();
	}
}

nothrow @nogc pure:

/**
 * MessagePack type-information format
 *
 * See_Also:
 *  $(LINK2 http://redmine.msgpack.org/projects/msgpack/wiki/FormatSpec, MessagePack Specificaton)
 */
enum Format : ubyte
{
	NONE,

	// unsinged integer
	UINT8  = 0xcc,  // ubyte
	UINT16 = 0xcd,  // ushort
	UINT32 = 0xce,  // uint
	UINT64 = 0xcf,  // ulong

	// signed integer
	INT8  = 0xd0,   // byte
	INT16 = 0xd1,   // short
	INT32 = 0xd2,   // int
	INT64 = 0xd3,   // long

	// floating point
	FLOAT  = 0xca,  // float
	DOUBLE = 0xcb,  // double

	// raw byte
	RAW   = 0xa0,
	RAW16 = 0xda,
	RAW32 = 0xdb,

	// bin type
	BIN8  = 0xc4,
	BIN16 = 0xc5,
	BIN32 = 0xc6,

	// ext type
	EXT   = 0xd4,  // fixext 1/2/4/8/16
	EXT8  = 0xc7,
	EXT16 = 0xc8,
	EXT32 = 0xc9,

	// str type
	STR8  = 0xd9,
	//STR16 = 0xda,
	//STR32 = 0xdb,

	// array
	ARRAY   = 0x90,
	ARRAY16 = 0xdc,
	ARRAY32 = 0xdd,

	// map
	MAP   = 0x80,
	MAP16 = 0xde,
	MAP32 = 0xdf,

	// other
	NIL   = 0xc0,   // null
	TRUE  = 0xc3,
	FALSE = 0xc2,

	// real (This format is D only!)
	REAL = 0xd4
}

/*
 * Calculates the format size of container length.
 */
size_t calculateSize(bool rawType = false)(in size_t length)
{
	static if (rawType)
		return length < 32 ? 0 : length < 65536 ? ushort.sizeof : uint.sizeof;
	else
		return length < 16 ? 0 : length < 65536 ? ushort.sizeof : uint.sizeof;
}

/// Adaptive Output Buffer
struct AOutputBuf(Stream, T = ubyte) if(isOutputBuffer!(Stream, T))
{
	@property ref Stream buf()
	{
		return *arr;
	}

	@property ref const(Stream) buf() const
	{
		return *arr;
	}

	Stream* arr;
	this(ref Stream array) { arr = &array; }

	ref const(Stream) opOpAssign(string op : "~")(inout(T[]) rhs) {
		buf.reserve(buf.length + rhs.length);
		foreach(ref elem; rhs)
			this ~= elem;
		return buf;
	}

	ref const(Stream) opOpAssign(string op : "~", U)(U rhs) if(!isDynamicArray!U) {
		static if (isPointer!Stream)
			size_t sz = 0;
		else
			size_t sz = buf.length;
		static if (__traits(compiles, buf.length = buf.length + 1))
			buf.length = sz + U.sizeof;
		else {
			// for Dvector!T
			buf.reserve(sz + U.sizeof);
			buf = Stream(&buf[0], sz + U.sizeof, buf.capacity);
		}
		*cast(Unqual!U*)&buf[sz] = rhs;
		static if (isPointer!Stream) buf += U.sizeof;
		return buf;
	}

	// for Array!T
	static if (!__traits(compiles, buf[0 .. 2] == [0, 1])) {
		T[] opSlice()
		{
			return (&buf[0])[0..buf.length];
		}

		T[] opSlice(size_t i, size_t j)
		{
			return (&buf[0])[i..j];
		}
	}

	alias buf this;
}

version(unittest)
{
	import std.container.array;
	import std.meta;
	import std.file, core.stdc.string;

	package:
	mixin template DefinePacker()
	{
		auto arr = Array!ubyte();
		auto packer = Packer!(Array!ubyte)(arr);
	}
	mixin template TestUnpacker()
	{
		auto unpacker = Unpacker!()(packer.buf[]);
		static if (__traits(compiles, typeof(test))) {
			auto result = unpacker.unpack!(typeof(test));
			auto testfunc = {
				import std.conv : text;
				assert(result == test, text(test, "\nExpected: ", result));
				return 0;
			}();
		}
	}
}