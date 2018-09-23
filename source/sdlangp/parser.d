/**** Parser after grammar splitting.
  * 
  * Author: ARaspiK
  * License: MIT
  */
module sdlangp.parser;

import pegged.grammar, sdlangp.grammar;
import sdlangp.node;

import std.array;
import std.datetime;
import std.algorithm;
import std.exception;
import std.base64;
import std.conv;
import std.bigint;
import sumtype;

/// Value parser for our custom grammar.
@trusted Value parseValue(ParseTree node) {
  // Get the actual type if not already done so.
  if (node.name == "SDL.Value")
    node = node.children[0];

  // Simpler access for most value types, by first match string.
  string data = node.matches[0];

  // Switch by name (excluding "SDL.")
  switch (node.name[4 .. $]) {
    case "String":
      return Value(data.idup);
    case "Number":
      // ...: 32-bit
      // ...L: 64-bit
      // ...BD: 128-bit
      return data[$-1] == 'L'
        ? Value(data[0 .. $-1].to!long)
        : data[$-2] == 'D'
        ? Value(BigInt(data[0 .. $-2]))
        : Value(data.to!int);
    case "Float":
      // ...: 64-bit
      // ...f: 32-bit
      return data[$-1] == 'f'
        ? Value(data[0 .. $-1].to!float)
        : Value(data.to!double);
    case "Boolean":
      // "on", "off", "true", "false"
      return Value(data == "true" || data == "on");
    case "Null":
      // "null"
      return Value(null);
    case "DateTime":
      // YYYY/MM/DD HH:MM:SS.FFF(-UTC)? => YYYYMMDD HHMMSS FFF (-UTC)?
      auto date = DateTime.min;
      auto frac = Duration.zero;
      bool utc = false;
      foreach_reverse(i, s; node.matches) switch (i) {
        case 3:
          utc = true;
          break;
        case 2:
          frac = s.to!size_t.msecs;
          break;
        case 1:
          date.timeOfDay = TimeOfDay.fromISOString(s);
          break;
        case 0:
          date.date = Date.fromISOString(s);
          break;
        default: assert(0);
      }
      return Value(SysTime(date, frac, utc ? UTC() : null));
    case "Duration":
      // (DDd:)?HH:MM:SS(.FFF)? => (DDd)? HH:MM:SS (.FFF)?
      auto d = Duration.zero;
      foreach (s; node.matches) {
        if (s[$-1] == 'd')
          d += s[0 .. $-1].to!size_t.days;
        else if (s[0] == '.')
          d += s[1 .. $].to!size_t.msecs;
        else
          d += TimeOfDay.fromISOString(s) - TimeOfDay.min;
      }
      return Value(d);
    case "Base64":
      // [base64stuff] => base64stuff
      return Value(Base64Impl!('+', '/', Base64.NoPadding).decode(data));
    default:
      assert(false, "No such value type!");
  }
}

/// Custom parser for attributes.
@safe Attribute parseAttribute(ParseTree node) nothrow {
  return Attribute(node.children[0].matches[0].idup,
      node.children[1].parseValue.assumeWontThrow);
}

/// Custom parser for node trees.
@safe Node*[] parseTree(ParseTree node) nothrow {
  return node.children.map!(l => l.children.map!parseNode).join;
}

/// Custom parser for single nodes.
@safe Node* parseNode(ParseTree node) nothrow {
  Node* res = new Node(null);

  foreach (t; node.children) switch (t.name[4 .. $]) {
    case "TagName":
      res.name = t.matches[$-1];
      res.namespace = t.matches.length == 2 ? t.matches[0] : "";
      break;
    case "Value":
      res.values ~= t.parseValue.assumeWontThrow;
      break;
    case "Attribute":
      res.attrs ~= t.parseAttribute;
      break;
    case "TagTree":
      res.children = t.parseTree;
      break;
    default: assert(0);
  }

  return res;
}
