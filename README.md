# `SDLangP`: A Pegged-based SDLang parser

The primary SDLang parser in D is `sdlang-d`, which does not (at the moment) provide
support for `@safe @nogc nothrow pure` attributes at all. As such, here is a simple
replacement which uses Pegged grammar. The grammar is hand-verified to work with the
example code on the [SDLang website][sdlang], which also provided here as `example.sdl`.
  
At the moment, this only provides read-only support (as that's what Pegged does).

##### MIT License
Copyright (c) 2018 ARaspiK

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

[sdlang]: https://sdlang.org
