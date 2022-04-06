# lmpl4d
Lightweight MessagePack library for D

# Features

* Small size and high performance
* Supports D features (Ranges, Tuples, real type)

# Usage

```d
import lmpl4d;

struct S { int id; string name; double size; }

ubyte[] buf;
auto data = packer(buf);
data.pack(42);
data.pack(null, true, 1u);
data.pack([1, 2, 3]);
data.pack(["foo": "bar"]);
data.pack(S(2, "foo", 7.5));
data.packArray(true, 1);  // -> [true, 1]
data.packMap("Hi", 100);  // -> ["Hi":100]
auto p = unpacker(data[]);
assert(p.unpack!int == 42);
assert(p.unpack!(int[]) == []);
assert(p.unpack!bool);
assert(p.unpack!uint == 1);
assert(p.unpack!(uint[])); // implicitly converts int[] to uint[]
assert(p.unpack!(string[string]));
```

## `nonPacked` attribute
Skip serialization/deserialization of a specific field
```d
struct User
{
    string name;
    @nonPacked int id;  // pack / unpack will ignore the 'id' field
}
```
