module lmpl4d.common;

import core.bitop, std.container.array;

// dfmt off
package import
	std.meta,
	std.traits,
	std.typecons;

version(EnableReal)
	enum EnableReal = true;
else
	enum EnableReal = false;

version(NoPackingStruct) {}
else {
	struct nonPacked {} // @suppress(dscanner.style.phobos_naming_convention)

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
 * For float/double type (de)serialization
 */
union _f { float f; uint i; }

union _d { double f; ulong i; }

// dfmt on

/**
 * For real type (de)serialization
 *
 * 80-bit real is padded to 12 bytes(Linux) and 16 bytes(Mac).
 * http://lists.puremagic.com/pipermail/digitalmars-d/2010-June/077394.html
 */
union _r {
	real f;

	struct {
		ulong fraction;
		ushort exponent; // includes sign
	}
}

enum isRawByte(T) = is(Unqual!T : ubyte) || isSomeChar!T;

/**
 * Gets asterisk string from pointer type
 */
template AsteriskOf(T) {
	static if (is(T P == U*, U))
		enum AsteriskOf = "*" ~ AsteriskOf!U;
	else
		enum AsteriskOf = "";
}

version (unittest) void fillData(T)(ref T data) {
	import core.stdc.stdlib;
	import std.datetime.systime;

	srand(cast(uint)Clock.currStdTime);
	foreach (ref x; data)
		x = cast(ubyte)rand();
}

public:

T toBE(T)(in T value) @trusted if (isIntegral!T && T.sizeof > 1) {
	version (LittleEndian) {
		static if (T.sizeof == 2)
			return byteswap(value);
		else static if (T.sizeof == 4)
			return bswap(cast(uint)value);
		else
			return bswap(value);
	} else
		return value;
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
ubyte take8from(T)(T value) {
	return cast(ubyte)value;
}

unittest {
	foreach (Int; AliasSeq!(ubyte, ushort, uint, ulong)) {
		assert(take8from(cast(Int)0x01) == 0x01);
		assert(take8from(cast(Int)0x0123) == 0x23);
		assert(take8from(cast(Int)0x01234567) == 0x67);
		assert(take8from(cast(Int)0x0123456789abcdef) == 0xef);
	}
}

enum isInputBuffer(R, E) = __traits(compiles, (R r, size_t i) { E e = r[i++]; });
enum isOutputBuffer(R, E) =
	__traits(compiles, (R r, E e) { r.reserve(1); r ~= e; r = R(&r[0], 1, 1); }) || __traits(
		compiles,
		(R r, E e) {
		size_t sz = r.length;
		r.length = sz + E.sizeof;
		*cast(Unqual!E*)&r[sz] = e;
	})
	|| __traits(compiles, (R r, E e) {
		*cast(Unqual!E*)&r[0] = e;
		r += E.sizeof;
	});

unittest {
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

version (betterC) {
} else {
pure:
	/**
	 * $(D MessagePackException) is a root Exception for MessagePack related operation.
	 */
	class MessagePackException : Exception {
		this(string msg) {
			super(msg);
		}
	}
	/**
	 * $(D UnpackException) is thrown on deserialization failure
	 */
	class UnpackException : MessagePackException {
		this(string msg) {
			super(msg);
		}
	}
}

nothrow @nogc pure:

/**
 * MessagePack type-information format
 *
 * See_Also:
 *  $(LINK2 http://redmine.msgpack.org/projects/msgpack/wiki/FormatSpec, MessagePack Specificaton)
 */
enum Format : ubyte {
	NONE,

	// unsinged integer
	UINT8 = 0xcc, // ubyte
	UINT16 = 0xcd, // ushort
	UINT32 = 0xce, // uint
	UINT64 = 0xcf, // ulong

	// signed integer
	INT8 = 0xd0, // byte
	INT16 = 0xd1, // short
	INT32 = 0xd2, // int
	INT64 = 0xd3, // long

	// floating point
	FLOAT = 0xca, // float
	DOUBLE = 0xcb, // double

	// bin type
	BIN8 = 0xc4,
	BIN16 = 0xc5,
	BIN32 = 0xc6,

	// ext type
	EXT = 0xd4, // fixext 1/2/4/8/16
	EXT8 = 0xc7,
	EXT16 = 0xc8,
	EXT32 = 0xc9,

	// str type
	STR = 0xa0,
	STR8 = 0xd9,
	STR16 = 0xda,
	STR32 = 0xdb,

	// array
	ARRAY = 0x90,
	ARRAY16 = 0xdc,
	ARRAY32 = 0xdd,

	// map
	MAP = 0x80,
	MAP16 = 0xde,
	MAP32 = 0xdf,

	// other
	NIL = 0xc0, // null
	TRUE = 0xc3,
	FALSE = 0xc2,

	// real (This format is D only!)
	REAL = 0xd4
}

/*
 * Calculates the format size of container length.
 */
size_t calculateSize(in size_t length) {
	return length < 16 ? 0 : length <= ushort.max ? ushort.sizeof : uint.sizeof;
}

/// Adaptive Output Buffer
struct AOutputBuf(Stream, T = ubyte) if (isOutputBuffer!(Stream, T)) {
	@property ref Stream buf() {
		return *arr;
	}

	@property ref const(Stream) buf() const {
		return *arr;
	}

	Stream* arr;
	this(ref Stream array) {
		arr = &array;
	}

	static if (!isDynamicArray!T)
		ref const(Stream) opOpAssign(string op : "~")(inout(T[]) rhs) {
			buf.reserve(buf.length + rhs.length);
			foreach (elem; rhs)
				this ~= elem;
			return buf;
		}

	ref const(Stream) opOpAssign(string op : "~", U)(in U rhs)
	if (!isDynamicArray!U) {
		static if (isPointer!Stream)
			size_t sz;
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
		static if (isPointer!Stream)
			buf += U.sizeof;
		return buf;
	}

	// for Array!T
	static if (!__traits(compiles, buf[0 .. 2] == [0, 1]))
		@nogc {
			T[] opSlice() {
				return (&buf[0])[0 .. buf.length];
			}

			T[] opSlice(size_t i, size_t j) {
				return (&buf[0])[i .. j];
			}
		}

	alias buf this;
}

version (unittest) {
	import std.container.array, core.stdc.string;

package:
	template DefinePacker() {
		auto arr = Array!ubyte();
		auto packer = Packer!(Array!ubyte)(arr);
	}

	template TestUnpacker() {
		auto unpacker = Unpacker!()(packer[]);
		static if (is(typeof(test))) {
			auto result = unpacker.unpack!(typeof(test));
			auto testfunc = {
				import std.conv : text;

				assert(result == test, text(test, "\nExpected: ", result));
				return 0;
			}();
		}
	}
}
