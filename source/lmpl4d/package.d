module lmpl4d;

public import lmpl4d.common, lmpl4d.packer, lmpl4d.unpacker;

auto packer(Stream)(ref Stream stream) {
	return Packer!Stream(stream);
}

auto unpacker(Stream = const(ubyte)[])(Stream stream) {
	return Unpacker!Stream(stream);
}

unittest {
	import std.stdio;

	ubyte[] buf;
	auto data = packer(buf);
	data.pack(42);
	data.pack(null, true, 1u);
	data.pack([1, 2, 3]);
	data.pack(["foo": "bar"]);
	data.packArray(true, 1);
	data.packMap("Hi", 100);
	auto p = unpacker(buf);
	assert(p.unpack!int == 42);
	assert(p.unpack!(int[]) == []);
	assert(p.unpack!bool);
	assert(p.unpack!uint == 1);
	assert(p.unpack!(uint[]));
	assert(p.unpack!(string[string]));
}
