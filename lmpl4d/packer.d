module lmpl4d.packer;

import lmpl4d.common;
import std.meta;

struct Packer(Stream = ubyte[]) if (isOutputBuffer!(Stream, ubyte)) {
	AOutputBuf!Stream buf;

	@property auto opSlice() => buf[];

	private alias TThis = typeof(this);

	this(ref Stream stream) {
		buf = AOutputBuf!Stream(stream);
	}

	/**
	 * Serializes argument and writes to stream.
	 *
	 * If the argument is the pointer type, dereferences the pointer and serializes pointed value.
	 * -----
	 * int  a = 10;
	 * int* b = &b;
	 *
	 * packer.pack(b);  // serializes 10, not address of a
	 * -----
	 * Serializes nil if the argument of nullable type is null.
	 *
	 * NOTE:
	 *  MessagePack doesn't define $(D_KEYWORD real) type format.
	 *  Don't serialize $(D_KEYWORD real) if you communicate with other languages.
	 *  Transfer $(D_KEYWORD double) serialization if $(D_KEYWORD real) on your environment equals $(D_KEYWORD double).
	 *
	 * Params:
	 *  value = the content to serialize.
	 *
	 * Returns:
	 *  self, i.e. for method chaining.
	 */
	ref TThis pack(bool value) {
		buf ~= value ? Format.TRUE : Format.FALSE;
		return this;
	}

	/// ditto
	ref TThis pack(T)(T value) if (isUnsigned!T && !is(T == enum)) {
		if (value < (1 << 8)) {
			if (value < (1 << 7)) {
				// fixnum
				buf ~= cast(ubyte)value;
			} else {
				buf ~= Format.UINT8;
				buf ~= cast(ubyte)value;
			}
		} else if (value < (1 << 16)) {
			buf ~= Format.UINT16;
			buf ~= toBE(cast(ushort)value);
		} else // ulong < ulong is slower than uint < uint
			static if (T.sizeof == 8) {
				if (value < (1UL << 32)) {
					buf ~= Format.UINT32;
					buf ~= toBE(cast(uint)value);
				} else {
					buf ~= Format.UINT64;
					buf ~= toBE(cast(ulong)value);
				}
			} else {
				buf ~= Format.UINT32;
				buf ~= toBE(cast(uint)value);
			}
		return this;
	}

	/// ditto
	ref TThis pack(T)(T value)
	if (isSigned!T && isIntegral!T && !is(T == enum)) {
		if (value < -(1 << 5)) {
			if (value < -(1 << 15)) {
				static if (T.sizeof == 8) {
					if (value < -(1L << 31)) {
						buf ~= Format.INT64;
						buf ~= toBE(cast(ulong)value);
						return this;
					}
				}
				buf ~= Format.INT32;
				buf ~= toBE(cast(uint)value);
			} else if (value < -(1 << 7)) {
				buf ~= Format.INT16;
				buf ~= toBE(cast(ushort)value);
			} else {
				buf ~= Format.INT8;
				buf ~= cast(ubyte)value;
			}
		} else if (value < (1 << 7)) {
			// fixnum
			buf ~= cast(ubyte)value;
		} else static if (T.sizeof == 8) {
			if (value < (1L << 16)) {
				if (value < (1L << 8)) {
					buf ~= Format.UINT8;
					buf ~= cast(ubyte)value;
				} else {
					buf ~= Format.UINT16;
					buf ~= toBE(cast(ushort)value);
				}
			} else if (value < (1L << 32)) {
				buf ~= Format.UINT32;
				buf ~= toBE(cast(uint)value);
			} else {
				buf ~= Format.UINT64;
				buf ~= toBE(cast(ulong)value);
			}
		} else {
			if (value < (1 << 8)) {
				buf ~= Format.UINT8;
				buf ~= cast(ubyte)value;
			} else if (value < (1 << 16)) {
				buf ~= Format.UINT16;
				buf ~= toBE(cast(ushort)value);
			} else {
				buf ~= Format.UINT32;
				buf ~= toBE(cast(uint)value);
			}
		}

		return this;
	}

	/// ditto
	ref TThis pack(T)(in T value) if (isFloatingPoint!T && !is(T == enum)) {
		static if (is(Unqual!T == float)) {
			buf ~= Format.FLOAT;
			buf ~= toBE(_f(value).i);
		} else static if (is(Unqual!T == double) || !EnableReal
			|| real.sizeof == double.sizeof) { // Non-x86 CPUs, real type equals double type.
			buf ~= Format.DOUBLE;
			buf ~= toBE(_d(value).i);
		} else {
			buf ~= Format.REAL;
			const tmp = _r(value);
			buf ~= toBE(tmp.fraction);
			buf ~= toBE(tmp.exponent);
		}

		return this;
	}

	ref TThis pack(T)(in T value) if (is(T == enum))
		=> pack(cast(OriginalType!T)value);

	/*
	 * Serializes the nil value.
	*/
	ref TThis pack(typeof(null)) {
		buf ~= Format.NIL;
		return this;
	}

	ref TThis pack(T)(in T value) if (isPointer!T) {
		if (value is null)
			buf ~= Format.NIL;
		else
			pack(mixin(AsteriskOf!T ~ "value"));
		return this;
	}

	/// ditto
	ref TThis pack(T)(in T array) if (isSomeArray!T) {
		import std.range;

		if (array.empty)
			return pack(null);

		// Raw bytes
		static if (typeof(T.init[0]).sizeof == 1) {
			static if (is(T : const(char)[]))
				beginStr(array.length);
			else
				beginBin(array.length);
			buf ~= cast(const(ubyte)[])array;
		} else {
			beginArray(array.length);
			foreach (elem; array)
				pack(elem);
		}
		return this;
	}

	/// ditto
	ref TThis pack(T)(in T array) if (isAssociativeArray!T) {
		if (!array)
			return pack(null);

		beginMap(array.length);
		foreach (key, value; array) {
			pack(key);
			pack(value);
		}

		return this;
	}

	/// ditto
	ref TThis pack(Types...)(auto ref const Types objects) if (Types.length > 1) {
		foreach (i, T; Types)
			pack(objects[i]);

		return this;
	}

	/// ditto
	ref TThis pack(T)(in T value) if (isSomeChar!T && !is(T == enum))
		=> pack(cast(AliasSeq!(ubyte, ushort, uint)[T.sizeof / 2])value);

	version (NoPackingStruct) {
	} else {
		ref TThis pack(T)(in T obj) if (is(T == struct)) {
			if (cast(const)T.init == obj) {
				beginArray(0);
				return this;
			}
			beginArray(NumOfSerializingMembers!T);
			foreach (f; obj.tupleof)
				static if (isPackedField!f && __traits(compiles, { pack(f); }))
					pack(f);
			return this;
		}
	}

	/**
	 * Serializes the arguments as container to buf.
	 *
	 * -----
	 * packer.packArray(true, 1);  // -> [true, 1]
	 * packer.packMap("Hi", 100);  // -> ["Hi":100]
	 * -----
	 *
	 * In packMap, the number of arguments must be even.
	 *
	 * Params:
	 *  objects = the contents to serialize.
	 *
	 * Returns:
	 *  self, i.e. for method chaining.
	 */
	ref TThis packArray(Types...)(auto ref const Types objects) {
		beginArray(Types.length);
		foreach (i, T; Types)
			pack(objects[i]);
		//pack(objects);  // slow :(

		return this;
	}

	/// ditto
	ref TThis packMap(Types...)(auto ref const Types objects)
	if ((Types.length & 1) == 0) {
		beginMap(Types.length >> 1);
		foreach (i, T; Types)
			pack(objects[i]);

		return this;
	}

	ref TThis packExt(in byte type, const ubyte[] data) return
	in (data.length <= uint.max) {
		ref TThis packExtFixed(int fmt) {
			buf ~= cast(ubyte)fmt;
			buf ~= type;
			buf ~= data;
			return this;
		}

		// Try packing to a fixed-length type
		switch (data.length) {
		case 1:
			return packExtFixed(Format.EXT);
		case 2:
			return packExtFixed(Format.EXT + 1);
		case 4:
			return packExtFixed(Format.EXT + 2);
		case 8:
			return packExtFixed(Format.EXT + 3);
		case 16:
			return packExtFixed(Format.EXT + 4);
		default:
		}

		begin!(Format.EXT8, Format.EXT16, 0)(data.length);
		buf ~= type;
		buf ~= data;

		return this;
	}

	ref TThis begin(Format f, Format f16, size_t llen = 16)(size_t len)
	in (len <= uint.max, "Data is too large to pack") {
		if (len <= ushort.max) {
			static if (llen) {
				if (len < llen) {
					buf ~= cast(ubyte)(f | cast(ubyte)len);
					return this;
				}
			}
			static if (f != Format.ARRAY && f != Format.MAP) {
				if (len <= ubyte.max) {
					buf ~= cast(Format)(f16 - 1);
					buf ~= cast(ubyte)len;
					return this;
				}
			}
			buf ~= f16;
			buf ~= toBE(cast(ushort)len);
		} else {
			buf ~= cast(Format)(f16 + 1);
			buf ~= toBE(cast(uint)len);
		}

		return this;
	}

	/*
	 * Serializes raw type-information to buf for binary type.
	 */
	alias beginBin = begin!(Format.BIN8, Format.BIN16, 0);

	/// ditto
	alias beginStr = begin!(Format.STR, Format.STR16, 32);

	/// ditto
	alias beginArray = begin!(Format.ARRAY, Format.ARRAY16);

	/// ditto
	alias beginMap = begin!(Format.MAP, Format.MAP16);
}

unittest  // unique value
{
	mixin DefinePacker;

	packer.pack(null, true, false);
	assert(packer[] == cast(ubyte[])[Format.NIL, Format.TRUE, Format.FALSE]);
}
// dfmt off
unittest {
	{ // uint *
		struct UTest { ubyte format; ulong value; }

		enum : ulong { A = ubyte.max, B = ushort.max, C = uint.max, D = ulong.max }

		enum UTest[][] utests = [
			[{Format.UINT8, A}],
			[{Format.UINT8, A}, {Format.UINT16, B}],
			[{Format.UINT8, A}, {Format.UINT16, B}, {Format.UINT32, C}],
			[{Format.UINT8, A}, {Format.UINT16, B}, {Format.UINT32, C}, {Format.UINT64, D}],
		];
		alias Types = AliasSeq!(ubyte, ushort, uint, ulong);

		foreach (I, T; Types) {
			static foreach (i, test; utests[I]) {{
				mixin DefinePacker;

				packer.pack(cast(T)test.value);
				assert(packer.buf[0] == test.format);

				const answer = toBE(cast(Types[i])test.value);
				assert(memcmp(&packer.buf[1], &answer, Types[i].sizeof) == 0);
			}}
		}
	}
	{ // int *
		struct STest { ubyte format; long value; }

		enum : long { A = byte.min, B = short.min, C = int.min, D = long.min }

		enum STest[][] stests = [
			[{Format.INT8, A}],
			[{Format.INT8, A}, {Format.INT16, B}],
			[{Format.INT8, A}, {Format.INT16, B}, {Format.INT32, C}],
			[{Format.INT8, A}, {Format.INT16, B}, {Format.INT32, C}, {Format.INT64, D}],
		];
		alias Types = AliasSeq!(byte, short, int, long);

		foreach (I, T; Types) {
			static foreach (i, test; stests[I]) {{
				mixin DefinePacker;

				packer.pack(cast(T)test.value);
				assert(packer.buf[0] == test.format);

				const answer = toBE(cast(Types[i])test.value);
				assert(memcmp(&packer.buf[1], &answer, Types[i].sizeof) == 0);
			}}
		}
	}
}

unittest { // float, double
	static if ((real.sizeof == double.sizeof) || !EnableReal) {
		alias FloatingTypes = AliasSeq!(float, double);
		struct FTest { ubyte format; double value; }

		enum FTest[] ftests = [
				{Format.FLOAT, float.min_normal},
				{Format.DOUBLE, double.max},
			];
	} else {
		alias FloatingTypes = AliasSeq!(float, double, real);
		struct FTest { ubyte format; real value; }

		enum FTest[] ftests = [
				{Format.FLOAT, float.min_normal},
				{Format.DOUBLE, double.max},
				{Format.REAL, real.max},
			];
	}

	foreach (I, T; FloatingTypes) {
		mixin DefinePacker;

		packer.pack(cast(T)ftests[I].value);
		assert(packer.buf[0] == ftests[I].format);

		switch (I) {
		case 0:
			const answer = toBE(_f(cast(T)ftests[I].value).i);
			assert(memcmp(&packer.buf[1], &answer, float.sizeof) == 0);
			break;
		case 1:
			const answer = toBE(_d(cast(T)ftests[I].value).i);
			assert(memcmp(&packer.buf[1], &answer, double.sizeof) == 0);
			break;
		default:
			version (EnableReal) {
				const t = _r(cast(T)ftests[I].value);
				const f = toBE(t.fraction);
				const e = toBE(t.exponent);
				assert(memcmp(&packer.buf[1], &f, f.sizeof) == 0);
				assert(memcmp(&packer.buf[1 + f.sizeof], &e, e.sizeof) == 0);
			} else {
				const answer = toBE(_d(cast(T)ftests[I].value).i);
				assert(memcmp(&packer.buf[1], &answer, double.sizeof) == 0);
			}
		}
	}
}

unittest  // pointer
{
	struct PTest {
		ubyte format;

		union {
			ulong* p0;
			long* p1;
			double* p2;
		}
	}

	PTest[3] ptests = [PTest(Format.UINT64), PTest(Format.INT64), PTest(Format.DOUBLE)];

	auto v0 = ulong.max;
	auto v1 = long.min;
	auto v2 = double.max;

	foreach (I; AliasSeq!(0, 1, 2)) {
		mixin DefinePacker;

		mixin("ptests[I].p", I, " = &v", I, ";");

		packer.pack(mixin("ptests[I].p", I));
		assert(packer.buf[0] == ptests[I].format);

		switch (I) {
		case 0:
			const answer = toBE(cast(ulong)*ptests[I].p0);
			assert(memcmp(&packer.buf[1], &answer, ulong.sizeof) == 0);
			break;
		case 1:
			const answer = toBE(cast(ulong)*ptests[I].p1);
			assert(memcmp(&packer.buf[1], &answer, ulong.sizeof) == 0);
			break;
		default:
			const answer = toBE(cast(ulong)_d(*ptests[I].p2).i);
			assert(memcmp(&packer.buf[1], &answer, ulong.sizeof) == 0);
		}
	}
}

unittest {
	{ // enum
		enum E : ubyte { A = ubyte.max }

		mixin DefinePacker;
		E e = E.A;

		packer.pack(e);
		assert(packer.buf[0] == Format.UINT8);

		auto answer = E.A;
		assert(memcmp(&packer.buf[1], &answer, (OriginalType!E).sizeof) == 0);
	}
	{ // enum with string
		enum E2 : string { A = "test" }

		mixin DefinePacker;
		E2 e = E2.A;

		packer.pack(e);
		assert(packer.buf[0] == (Format.STR | 0x04));
	}
}

unittest {
	// container
	struct CTest { ubyte format; size_t value; }

	enum : ulong { A = 16 / 2, B = ushort.max, C = uint.max }

	immutable CTest[3][3] ctests = [
		[{Format.ARRAY | A, Format.ARRAY | A}, {Format.ARRAY16, B}, {Format.ARRAY32, C}],
		[{Format.MAP   | A, Format.MAP   | A}, {Format.MAP16,   B}, {Format.MAP32,   C}],
		[{Format.STR   | A, Format.STR   | A}, {Format.STR16,   B}, {Format.STR32,   C}],
		];

	foreach (I, Name; AliasSeq!("Array", "Map", "Str")) {
		auto test = ctests[I];
		alias Types = AliasSeq!(ubyte, ushort, uint);

		foreach (i, T; Types) {
			mixin DefinePacker;
			mixin("packer.begin" ~ Name ~ "(i ? test[i].value : A);");

			assert(packer.buf[0] == test[i].format);

			const answer = toBE(cast(T)test[i].value);
			assert(memcmp(&packer.buf[i ? 1 : 0], &answer, T.sizeof) == 0);
		}
	}

// dfmt on
version (D_Exceptions) { // ext types
	import std.conv : text;

	byte type = 7; // an arbitrary type value

	// fixexts
	{
		ubyte[16] data = void;
		data.fillData;
		foreach (L; AliasSeq!(1, 2, 4, 8, 16)) {
			mixin DefinePacker;
			packer.packExt(type, data[0 .. L]);

			// format, type, data
			assert(packer.buf.length == 2 + L);
			const l = 1 << (packer.buf[0] - Format.EXT);
			assert(l == L);
			assert(packer.buf[1] == type);
			assert(packer.buf[2 .. 2 + l] == data[0 .. L]);
		}
	}

	ubyte[] data;
	// ext8
	foreach (L; AliasSeq!(3, 7, 255)) {
		data = new ubyte[L];
		data.fillData;

		mixin DefinePacker;
		packer.packExt(type, data[0 .. L]);

		// format, length, type, data
		assert(packer.buf.length == 3 + L);
		assert(packer.buf[0] == Format.EXT8);
		assert(packer.buf[1] == L);
		assert(packer.buf[2] == type);
		assert(packer.buf[3 .. 3 + L] == data);
	}

	// ext16
	foreach (L; AliasSeq!(256, 0xffff)) {
		data = new ubyte[L];
		data.fillData;

		mixin DefinePacker;
		packer.packExt(type, data[0 .. L]);

		// format, length, type, data
		assert(packer.buf.length == 4 + L, text(packer.buf.length));
		assert(packer.buf[0] == Format.EXT16);

		auto l = toBE(cast(ushort)L);
		assert(memcmp(&packer.buf[1], &l, ushort.sizeof) == 0);
		assert(packer.buf[3] == type);
		assert(packer.buf[4 .. 4 + L] == data);
	}

	// ext32
	foreach (L; AliasSeq!(2 ^^ 16, 2 ^^ 17)) {
		data = new ubyte[L];
		data.fillData;

		mixin DefinePacker;
		packer.packExt(type, data[0 .. L]);

		// format, length, type, data
		assert(packer.buf.length == 6 + L, text(packer.buf.length));
		assert(packer.buf[0] == Format.EXT32);

		auto l = toBE(cast(uint)L);
		assert(memcmp(&packer.buf[1], &l, uint.sizeof) == 0);
		assert(packer.buf[5] == type);
		assert(packer.buf[6 .. 6 + L] == data);
	}
}
}
