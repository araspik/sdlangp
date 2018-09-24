/**** Primary node type and associated types.
  * 
  * Author: ARaspiK
  * License: MIT
  */
module sdlangp.node;

import std.bigint;
import std.datetime;
import std.algorithm;
import sumtype;
import std.conv;
import std.exception;
import std.format;
import std.range;

/// The node type.
struct Node {

  Node* parent;

  string namespace, name;

  Value[] values;
  Value[string] attrs;

  Node*[] children;

  @safe this(Node* parent, string namespace = "", string name = "",
        Value[] values = [], Value[string] attrs = null, Node*[] children = [],
      ) nothrow pure {
    this.parent = parent;
    this.namespace = namespace;
    this.name = name;
    this.values = values;
    this.attrs = attrs;
    this.children = children;
    if (parent !is null)
      parent.children ~= &this;
  }

  @trusted string toString() const nothrow {
    import std.array;
    import std.string;

    auto res = appender!string;
    bool noname = false;

    if (namespace.length > 0)
      res.put(format!"%s:%s: "(namespace, name.length > 0 ? name : `""`)
          .assumeWontThrow);
    else if (name.length > 0)
      res.put(name.format!"%s: ".assumeWontThrow);
    else
      noname = true;

    res.put(
      choose(values.length + attrs.length > 0,
        values.map!(v => v.toString)
          .chain(attrs.byKeyValue
            .map!(a => format!"%s=%s"(a.key, a.value.toString).assumeWontThrow))
          .array
          .format!"%-(%s%|, %)\n".assumeWontThrow
          .only,
        choose(noname, "".only.takeNone, "\n".only))
      .chain(choose(noname,
            children.map!(c => c.toString),
            children.map!(c => c.toString
              .lineSplitter
              .map!(l => l.format!"  %s\n".assumeWontThrow).join)
          ))
      .join);
    return res.data;
  }

  @property @safe Value opIndex(size_t i) const nothrow pure {
    return values[i];
  }

  @property @safe Value opIndexAssign(Value v, size_t i) nothrow pure {
    return values[i] = v;
  }

  @property @safe Value opIndex(string name) const nothrow pure {
    return attrs[name];
  }

  @property @safe Value opIndexAssign(Value v, string name) nothrow pure {
    return attrs[name] = v;
  }
}

/// The value type.
alias Value = SumType!(
    int, long, BigInt,    // Number
    float, double,        // Float
    bool, typeof(null),   // Boolean and Null
    Duration, SysTime,    // Time
    string,               // Text
    ubyte[],              // Miscellaneous
  );

/// Converts the value to a string.
@trusted string toString(Value v) nothrow {
  return v.match!(t => t.to!string.assumeWontThrow);
}
