/**** Provides common functionality;
  * 
  * Author: ARaspiK
  * License: MIT
  */
module sdlangp;

public import sdlangp.node;

import sdlangp.parser, sdlangp.grammar;

@trusted Node* parseSource(string source) nothrow {
  import std.exception;

  return new Node(null, "", "", [], [],
      SDL(source).assumeWontThrow.children[0].parseTree);
}

@safe Node* parseFile(string name) {
  import std.file;

  return name.readText.parseSource;
}
