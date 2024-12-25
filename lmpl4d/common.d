module lmpl4d.common;

import std.container.array;

package import std.meta,
std.traits,
std.typecons;

version (NoPackingStruct) {
} else {
	enum nonPacked; // @suppress(dscanner.style.phobos_naming_convention)

	enum isPackedField(alias field) = staticIndexOf!(nonPacked,
			__traits(getAttributes, field)) == -1
		&& !isSomeFunction!field;

	/++
		Get the number of member to serialize.
	+/
	template NumOfSerializingMembers(T...) {
		static if (T.length)
			enum NumOfSerializingMembers = Filter!(isPackedField, T[0].tupleof).length +
				NumOfSerializingMembers!(T[1 .. $]);
		else
			enum NumOfSerializingMembers = 0;
	}
}

package:

version (EnableReal) {
	enum EnableReal = true;

	auto getReal(ulong frac, ushort exp) {
		static if (real.sizeof == double.sizeof) {
			// for 80bit real inter-operation on non-x86 CPU
			import std.numeric;

			CustomFloat!(64, 7) tmp;

			tmp.significand = frac;
			tmp.exponent = exp & 0x7fff;
			tmp.sign = (exp & 0x8000) != 0;

			// NOTE: tmp.get!real is inf on non-x86 when deserialized value is larger than double.max.
			return tmp.get!real;
		} else {
			_r tmp = {fraction: frac, exponent: exp};
			return tmp.f;
		}
	}
} else
	enum EnableReal = false;

version (unittest) {
	struct SimpleArray(T) {
		T* ptr;
		private size_t _length;
	nothrow:
		@disable this(this);

		~this() {
			length = 0;
		}

		@property size_t length() const => _length;
		@property void length(size_t n) {
			import core.checkedint : mulu;
			import core.exception;
			import core.stdc.stdlib;

			bool overflow;
			size_t reqsize = mulu(T.sizeof, n, overflow);
			if (!overflow) {
				_length = n;
				ptr = cast(T*)realloc(ptr, reqsize);
			} else
				onOutOfMemoryErrorNoGC();
		}

		alias opDollar = length;

		ref inout(T) opIndex(size_t idx) inout
		in (idx < _length)
			=> ptr[idx];

		inout(T)[] opSlice() inout => ptr[0 .. _length];

		inout(T)[] opSlice(size_t a, size_t b) inout
		in (a <= b && b <= _length)
			=> ptr[a .. b];
	}

	void fillData(T)(ref T data) {
		import core.stdc.stdlib;
		import core.stdc.time;

		srand(cast(uint)time(null));
		foreach (ref x; data)
			x = cast(ubyte)rand();
	}
}

enum isOutBuffer(R, E) = __traits(compiles, (R r, E e) {
		size_t sz = r.length;
		r.length = sz + E.sizeof;
		*cast(Unqual!E*)&r[sz] = e;
	})
	|| __traits(compiles, (R r, E e) {
		*cast(Unqual!E*)&r[0] = e;
		r += E.sizeof;
	});

version (LittleEndian)
	import std.bitmanip : toBE = swapEndian;
else
	T toBE(T)(in T value) if (isIntegral!T) => value;

	public:

	enum isInputBuffer(R, E) = __traits(compiles, (R r, size_t i) { E e = r[i++]; });
version (D_TypeInfo)
	enum isOutputBuffer(R, E) =
		__traits(compiles, (R r, E e) { r.reserve(1); r ~= e; r = R(&r[0], 1, 1); }) ||
		isOutBuffer!(R, E);
else
	alias isOutputBuffer = isOutBuffer;

unittest {
	static assert(!isInputBuffer!(void[], ubyte));
	static assert(isInputBuffer!(ubyte[9], ubyte));
	static assert(isInputBuffer!(ubyte[], ubyte));
	static assert(isInputBuffer!(ubyte*, ubyte));
	static assert(!isInputBuffer!(ubyte, ubyte));
	static assert(!isInputBuffer!(int[], ubyte));
	static assert(isOutputBuffer!(void[], ubyte));
	static assert(!isOutputBuffer!(ubyte[9], ubyte));
	static assert(isOutputBuffer!(ubyte[], ubyte));
	static assert(isOutputBuffer!(ubyte*, ubyte));
	static assert(isOutputBuffer!(int[], ubyte));
	version (D_BetterC) {
	} else {
		static assert(isInputBuffer!(Array!ubyte, ubyte));
		static assert(isOutputBuffer!(Array!ubyte, ubyte));
	}
}

version (D_Exceptions) {
	/++
		`MessagePackException` is a root Exception for MessagePack related operation.
	+/
	class MessagePackException : Exception {
		this(string msg) pure {
			super(msg);
		}
	}

	package alias Ex = MessagePackException;
}

nothrow @nogc pure:

/++
MessagePack type-information format

See_Also:
	[MessagePack Specificaton](http://redmine.msgpack.org/projects/msgpack/wiki/FormatSpec)
+/
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

/++
	Calculates the format size of container length.
+/
size_t calculateSize(size_t length)
	=> length < 16 ? 0 : length <= ushort.max ? ushort.sizeof : uint.sizeof;

/// Adaptive Output Buffer
struct AOutputBuf(Stream, T = ubyte) if (isOutputBuffer!(Stream, T)) {
	@property ref Stream buf() => *arr;

	@property ref const(Stream) buf() const => *arr;

	Stream* arr;
	this(ref Stream array) @trusted {
		arr = &array;
	}

	static if (!isDynamicArray!T)
		ref const(Stream) opOpAssign(string op : "~")(in T[] rhs) {
			static if (is(typeof(buf.reserve(buf.length))))
				buf.reserve(buf.length + rhs.length);
			foreach (elem; rhs)
				this ~= elem;
			return buf;
		}

	ref const(Stream) opOpAssign(string op : "~", U)(in U rhs)
	if (!isDynamicArray!U) {
		static if (isPointer!Stream)
			size_t sz;
		else {
			size_t sz = buf.length;
			static if (__traits(compiles, buf.length = buf.length + 1))
				buf.length = sz + U.sizeof;
			else {
				// for Dvector!T
				buf.reserve(sz + U.sizeof);
				buf = Stream(&buf[0], sz + U.sizeof, buf.capacity);
			}
		}
		*cast(Unqual!U*)&buf[sz] = rhs;
		static if (isPointer!Stream)
			buf += U.sizeof;
		return buf;
	}

	// for Array!T
	static if (!__traits(compiles, buf[0 .. 2] == [0, 1]))
		@nogc {
			T[] opSlice() => (&buf[0])[0 .. buf.length];

			T[] opSlice(size_t i, size_t j) => (&buf[0])[i .. j];
		}

	alias buf this;
}

package:
enum isSomeArray(T) = (isArray!T || isInstanceOf!(Array, T)) && !is(T == enum);

/++ For float/double type (de)serialization +/
union _f {
	float f;
	uint i;
}

union _d {
	double f;
	ulong i;
}

/++
For real type (de)serialization

80-bit real is padded to 12 bytes(Linux) and 16 bytes(Mac).
http://lists.puremagic.com/pipermail/digitalmars-d/2010-June/077394.html
+/
union _r {
	real f;

	struct {
		ulong fraction;
		ushort exponent; // includes sign
	}
}
/++
	Gets asterisk string from pointer type
+/
template AsteriskOf(T) {
	static if (is(T P == U*, U))
		enum AsteriskOf = '*' ~ AsteriskOf!U;
	else
		enum AsteriskOf = "";
}

version (unittest) {
	import core.stdc.string;

	template DefinePacker() {
		version (D_BetterC)
			alias Array = SimpleArray;
		else
			import std.container.array;

		auto arr = Array!ubyte();
		auto packer = Packer!(Array!ubyte)(arr);
	}

	template TestUnpacker() {
		auto unpacker = Unpacker!()(packer[]);
		static if (is(typeof(test))) {
			auto result = unpacker.unpack!(typeof(test));
			int testfunc = {
				version (D_BetterC)
					assert(result == test);
				else {
					import std.conv : text;

					assert(result == test, text(test, "\nExpected: ", result));
				}
				return 0;
			}();
		}
	}
}
