/**** Primary node type and associated types.
  * 
  * Author: ARaspiK
  * License: MIT
  */
module sdlangp.node;

import std.bigint;
import std.datetime;
import sumtype;

/// The node type.
struct Node {

  Node* parent;

  string namespace, name;

  Value[] values;
  Attribute[] attrs;

  Node*[] children;

  @safe this(Node* parent, string namespace = "", string name = "",
        Value[] values = [], Attribute[] attrs = [], Node*[] children = [],
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
}

/// The attribute type.
struct Attribute {
  string name;
  Value val;
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
