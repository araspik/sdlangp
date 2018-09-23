/++
This module was automatically generated from the following grammar:

SDL:
    TagTree  <- Line*
    Line     <  Tag (:';' Tag)* :EOL
    Tag      <  (Value / TagName) (Value :Spacing)* (Attribute Spacing)* (:'{' :EOL TagTree :'}')?

    TagName  <- ;Namespace? ~([a-z][a-zA-Z0-9_.$\-]*)?
    AttrName <- ~([a-zA-Z0-9_]+)
    Namespace <- ~([a-z][a-zA-Z0-9_.$\-]*)? :':'

    Value    <- String / DateTime / Duration / Float / Number / Base64 / Boolean / Null
    Attribute <- AttrName :'=' Value

    String   <~ :doublequote (:backslash doublequote / !doublequote !endOfLine .)* :doublequote
      / :backquote (!backquote !endOfLine .)* :backquote
    DateTime <  Date Time?
    Date     <~ Digit Digit Digit Digit :'/' Digit Digit :'/' Digit Digit
    Time     <- ~(Digit Digit :':' Digit Digit :':' Digit Digit) :'.' ~(Digit Digit Digit) UTC?
    Duration <- ~(Digit+ 'd' :':')? ~(Digit Digit :':' Digit Digit :':' Digit Digit) ~('.' Digit Digit Digit)?
    Number   <~ Digit+ ('L' / 'BD')?
    Float    <~ Digit+ '.' Digit+ 'f'?
    Boolean  <- "true" / "false" / "on" / "off"
    Null     <- "null"
    Base64   <- :'[' ~(([a-zA-Z+0-9/] / :Spacing)*) :']'
    UTC      <- "-UTC"

    Digit    <- [0-9]
    Char     <- !doublequote !endOfLine . / backslash doublequote
    Spacing  <- BlockComment / backslash endOfLine / :(' ' / '\t')*
    EOL      <: endOfLine / LineComment

    LineComment  <- :("//" / "--" / "#") (!endOfLine .)* endOfLine
    BlockComment <- "/*" (!"*/" .)* "*/"


+/
module sdlangp.grammar;

public import pegged.peg;
import std.algorithm: startsWith;
import std.functional: toDelegate;

struct GenericSDL(TParseTree)
{
    import std.functional : toDelegate;
    import pegged.dynamic.grammar;
    static import pegged.peg;
    struct SDL
    {
    enum name = "SDL";
    static ParseTree delegate(ParseTree)[string] before;
    static ParseTree delegate(ParseTree)[string] after;
    static ParseTree delegate(ParseTree)[string] rules;
    import std.typecons:Tuple, tuple;
    static TParseTree[Tuple!(string, size_t)] memo;
    static this()
    {
        rules["TagTree"] = toDelegate(&TagTree);
        rules["Line"] = toDelegate(&Line);
        rules["Tag"] = toDelegate(&Tag);
        rules["TagName"] = toDelegate(&TagName);
        rules["AttrName"] = toDelegate(&AttrName);
        rules["Namespace"] = toDelegate(&Namespace);
        rules["Value"] = toDelegate(&Value);
        rules["Attribute"] = toDelegate(&Attribute);
        rules["String"] = toDelegate(&String);
        rules["DateTime"] = toDelegate(&DateTime);
        rules["Date"] = toDelegate(&Date);
        rules["Time"] = toDelegate(&Time);
        rules["Duration"] = toDelegate(&Duration);
        rules["Number"] = toDelegate(&Number);
        rules["Float"] = toDelegate(&Float);
        rules["Boolean"] = toDelegate(&Boolean);
        rules["Null"] = toDelegate(&Null);
        rules["Base64"] = toDelegate(&Base64);
        rules["UTC"] = toDelegate(&UTC);
        rules["Digit"] = toDelegate(&Digit);
        rules["Char"] = toDelegate(&Char);
        rules["Spacing"] = toDelegate(&Spacing);
    }

    template hooked(alias r, string name)
    {
        static ParseTree hooked(ParseTree p)
        {
            ParseTree result;

            if (name in before)
            {
                result = before[name](p);
                if (result.successful)
                    return result;
            }

            result = r(p);
            if (result.successful || name !in after)
                return result;

            result = after[name](p);
            return result;
        }

        static ParseTree hooked(string input)
        {
            return hooked!(r, name)(ParseTree("",false,[],input));
        }
    }

    static void addRuleBefore(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar name
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(ruleName,rule; dg.rules)
            if (ruleName != "Spacing") // Keep the local Spacing rule, do not overwrite it
                rules[ruleName] = rule;
        before[parentRule] = rules[dg.startingRule];
    }

    static void addRuleAfter(string parentRule, string ruleSyntax)
    {
        // enum name is the current grammar named
        DynamicGrammar dg = pegged.dynamic.grammar.grammar(name ~ ": " ~ ruleSyntax, rules);
        foreach(name,rule; dg.rules)
        {
            if (name != "Spacing")
                rules[name] = rule;
        }
        after[parentRule] = rules[dg.startingRule];
    }

    static bool isRule(string s)
    {
		import std.algorithm : startsWith;
        return s.startsWith("SDL.");
    }
    mixin decimateTree;

    static TParseTree TagTree(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(Line), "SDL.TagTree")(p);
        }
        else
        {
            if (auto m = tuple(`TagTree`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(Line), "SDL.TagTree"), "TagTree")(p);
                memo[tuple(`TagTree`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TagTree(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.zeroOrMore!(Line), "SDL.TagTree")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.zeroOrMore!(Line), "SDL.TagTree"), "TagTree")(TParseTree("", false,[], s));
        }
    }
    static string TagTree(GetName g)
    {
        return "SDL.TagTree";
    }

    static TParseTree Line(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Tag, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, Tag, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing))), "SDL.Line")(p);
        }
        else
        {
            if (auto m = tuple(`Line`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Tag, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, Tag, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing))), "SDL.Line"), "Line")(p);
                memo[tuple(`Line`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Line(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Tag, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, Tag, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing))), "SDL.Line")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Tag, Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!(";"), Spacing)), pegged.peg.wrapAround!(Spacing, Tag, Spacing)), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing))), "SDL.Line"), "Line")(TParseTree("", false,[], s));
        }
    }
    static string Line(GetName g)
    {
        return "SDL.Line";
    }

    static TParseTree Tag(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.wrapAround!(Spacing, TagName, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing)), pegged.peg.wrapAround!(Spacing, TagTree, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing))), "SDL.Tag")(p);
        }
        else
        {
            if (auto m = tuple(`Tag`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.wrapAround!(Spacing, TagName, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing)), pegged.peg.wrapAround!(Spacing, TagTree, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing))), "SDL.Tag"), "Tag")(p);
                memo[tuple(`Tag`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Tag(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.wrapAround!(Spacing, TagName, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing)), pegged.peg.wrapAround!(Spacing, TagTree, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing))), "SDL.Tag")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, pegged.peg.or!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.wrapAround!(Spacing, TagName, Spacing)), Spacing), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Value, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, Spacing, Spacing))), Spacing)), pegged.peg.zeroOrMore!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Attribute, Spacing), pegged.peg.wrapAround!(Spacing, Spacing, Spacing)), Spacing)), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, pegged.peg.and!(pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("{"), Spacing)), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, EOL, Spacing)), pegged.peg.wrapAround!(Spacing, TagTree, Spacing), pegged.peg.discard!(pegged.peg.wrapAround!(Spacing, pegged.peg.literal!("}"), Spacing))), Spacing))), "SDL.Tag"), "Tag")(TParseTree("", false,[], s));
        }
    }
    static string Tag(GetName g)
    {
        return "SDL.Tag";
    }

    static TParseTree TagName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Namespace)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-"))))))), "SDL.TagName")(p);
        }
        else
        {
            if (auto m = tuple(`TagName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Namespace)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-"))))))), "SDL.TagName"), "TagName")(p);
                memo[tuple(`TagName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree TagName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Namespace)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-"))))))), "SDL.TagName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.drop!(pegged.peg.option!(Namespace)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-"))))))), "SDL.TagName"), "TagName")(TParseTree("", false,[], s));
        }
    }
    static string TagName(GetName g)
    {
        return "SDL.TagName";
    }

    static TParseTree AttrName(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))), "SDL.AttrName")(p);
        }
        else
        {
            if (auto m = tuple(`AttrName`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))), "SDL.AttrName"), "AttrName")(p);
                memo[tuple(`AttrName`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree AttrName(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))), "SDL.AttrName")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.oneOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_")))), "SDL.AttrName"), "AttrName")(TParseTree("", false,[], s));
        }
    }
    static string AttrName(GetName g)
    {
        return "SDL.AttrName";
    }

    static TParseTree Namespace(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-")))))), pegged.peg.discard!(pegged.peg.literal!(":"))), "SDL.Namespace")(p);
        }
        else
        {
            if (auto m = tuple(`Namespace`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-")))))), pegged.peg.discard!(pegged.peg.literal!(":"))), "SDL.Namespace"), "Namespace")(p);
                memo[tuple(`Namespace`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Namespace(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-")))))), pegged.peg.discard!(pegged.peg.literal!(":"))), "SDL.Namespace")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.charRange!('a', 'z'), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("_"), pegged.peg.literal!("."), pegged.peg.literal!("$"), pegged.peg.literal!("-")))))), pegged.peg.discard!(pegged.peg.literal!(":"))), "SDL.Namespace"), "Namespace")(TParseTree("", false,[], s));
        }
    }
    static string Namespace(GetName g)
    {
        return "SDL.Namespace";
    }

    static TParseTree Value(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(String, DateTime, Duration, Float, Number, Base64, Boolean, Null), "SDL.Value")(p);
        }
        else
        {
            if (auto m = tuple(`Value`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(String, DateTime, Duration, Float, Number, Base64, Boolean, Null), "SDL.Value"), "Value")(p);
                memo[tuple(`Value`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Value(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(String, DateTime, Duration, Float, Number, Base64, Boolean, Null), "SDL.Value")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(String, DateTime, Duration, Float, Number, Base64, Boolean, Null), "SDL.Value"), "Value")(TParseTree("", false,[], s));
        }
    }
    static string Value(GetName g)
    {
        return "SDL.Value";
    }

    static TParseTree Attribute(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(AttrName, pegged.peg.discard!(pegged.peg.literal!("=")), Value), "SDL.Attribute")(p);
        }
        else
        {
            if (auto m = tuple(`Attribute`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(AttrName, pegged.peg.discard!(pegged.peg.literal!("=")), Value), "SDL.Attribute"), "Attribute")(p);
                memo[tuple(`Attribute`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Attribute(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(AttrName, pegged.peg.discard!(pegged.peg.literal!("=")), Value), "SDL.Attribute")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(AttrName, pegged.peg.discard!(pegged.peg.literal!("=")), Value), "SDL.Attribute"), "Attribute")(TParseTree("", false,[], s));
        }
    }
    static string Attribute(GetName g)
    {
        return "SDL.Attribute";
    }

    static TParseTree String(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(backslash), doublequote), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(doublequote)), pegged.peg.and!(pegged.peg.discard!(backquote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(backquote)))), "SDL.String")(p);
        }
        else
        {
            if (auto m = tuple(`String`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(backslash), doublequote), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(doublequote)), pegged.peg.and!(pegged.peg.discard!(backquote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(backquote)))), "SDL.String"), "String")(p);
                memo[tuple(`String`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree String(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(backslash), doublequote), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(doublequote)), pegged.peg.and!(pegged.peg.discard!(backquote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(backquote)))), "SDL.String")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(doublequote), pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.and!(pegged.peg.discard!(backslash), doublequote), pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any))), pegged.peg.discard!(doublequote)), pegged.peg.and!(pegged.peg.discard!(backquote), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(backquote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), pegged.peg.discard!(backquote)))), "SDL.String"), "String")(TParseTree("", false,[], s));
        }
    }
    static string String(GetName g)
    {
        return "SDL.String";
    }

    static TParseTree DateTime(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Date, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Time, Spacing))), "SDL.DateTime")(p);
        }
        else
        {
            if (auto m = tuple(`DateTime`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Date, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Time, Spacing))), "SDL.DateTime"), "DateTime")(p);
                memo[tuple(`DateTime`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree DateTime(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Date, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Time, Spacing))), "SDL.DateTime")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.wrapAround!(Spacing, Date, Spacing), pegged.peg.option!(pegged.peg.wrapAround!(Spacing, Time, Spacing))), "SDL.DateTime"), "DateTime")(TParseTree("", false,[], s));
        }
    }
    static string DateTime(GetName g)
    {
        return "SDL.DateTime";
    }

    static TParseTree Date(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit)), "SDL.Date")(p);
        }
        else
        {
            if (auto m = tuple(`Date`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit)), "SDL.Date"), "Date")(p);
                memo[tuple(`Date`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Date(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit)), "SDL.Date")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!("/")), Digit, Digit)), "SDL.Date"), "Date")(TParseTree("", false,[], s));
        }
    }
    static string Date(GetName g)
    {
        return "SDL.Date";
    }

    static TParseTree Time(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.discard!(pegged.peg.literal!(".")), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit)), pegged.peg.option!(UTC)), "SDL.Time")(p);
        }
        else
        {
            if (auto m = tuple(`Time`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.discard!(pegged.peg.literal!(".")), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit)), pegged.peg.option!(UTC)), "SDL.Time"), "Time")(p);
                memo[tuple(`Time`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Time(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.discard!(pegged.peg.literal!(".")), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit)), pegged.peg.option!(UTC)), "SDL.Time")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.discard!(pegged.peg.literal!(".")), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, Digit)), pegged.peg.option!(UTC)), "SDL.Time"), "Time")(TParseTree("", false,[], s));
        }
    }
    static string Time(GetName g)
    {
        return "SDL.Time";
    }

    static TParseTree Duration(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("d"), pegged.peg.discard!(pegged.peg.literal!(":"))))), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Digit, Digit, Digit)))), "SDL.Duration")(p);
        }
        else
        {
            if (auto m = tuple(`Duration`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("d"), pegged.peg.discard!(pegged.peg.literal!(":"))))), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Digit, Digit, Digit)))), "SDL.Duration"), "Duration")(p);
                memo[tuple(`Duration`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Duration(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("d"), pegged.peg.discard!(pegged.peg.literal!(":"))))), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Digit, Digit, Digit)))), "SDL.Duration")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("d"), pegged.peg.discard!(pegged.peg.literal!(":"))))), pegged.peg.fuse!(pegged.peg.and!(Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit, pegged.peg.discard!(pegged.peg.literal!(":")), Digit, Digit)), pegged.peg.fuse!(pegged.peg.option!(pegged.peg.and!(pegged.peg.literal!("."), Digit, Digit, Digit)))), "SDL.Duration"), "Duration")(TParseTree("", false,[], s));
        }
    }
    static string Duration(GetName g)
    {
        return "SDL.Duration";
    }

    static TParseTree Number(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.keywords!("L", "BD")))), "SDL.Number")(p);
        }
        else
        {
            if (auto m = tuple(`Number`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.keywords!("L", "BD")))), "SDL.Number"), "Number")(p);
                memo[tuple(`Number`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Number(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.keywords!("L", "BD")))), "SDL.Number")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.keywords!("L", "BD")))), "SDL.Number"), "Number")(TParseTree("", false,[], s));
        }
    }
    static string Number(GetName g)
    {
        return "SDL.Number";
    }

    static TParseTree Float(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.literal!("f")))), "SDL.Float")(p);
        }
        else
        {
            if (auto m = tuple(`Float`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.literal!("f")))), "SDL.Float"), "Float")(p);
                memo[tuple(`Float`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Float(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.literal!("f")))), "SDL.Float")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.fuse!(pegged.peg.and!(pegged.peg.oneOrMore!(Digit), pegged.peg.literal!("."), pegged.peg.oneOrMore!(Digit), pegged.peg.option!(pegged.peg.literal!("f")))), "SDL.Float"), "Float")(TParseTree("", false,[], s));
        }
    }
    static string Float(GetName g)
    {
        return "SDL.Float";
    }

    static TParseTree Boolean(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("true", "false", "on", "off"), "SDL.Boolean")(p);
        }
        else
        {
            if (auto m = tuple(`Boolean`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.keywords!("true", "false", "on", "off"), "SDL.Boolean"), "Boolean")(p);
                memo[tuple(`Boolean`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Boolean(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.keywords!("true", "false", "on", "off"), "SDL.Boolean")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.keywords!("true", "false", "on", "off"), "SDL.Boolean"), "Boolean")(TParseTree("", false,[], s));
        }
    }
    static string Boolean(GetName g)
    {
        return "SDL.Boolean";
    }

    static TParseTree Null(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("null"), "SDL.Null")(p);
        }
        else
        {
            if (auto m = tuple(`Null`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("null"), "SDL.Null"), "Null")(p);
                memo[tuple(`Null`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Null(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("null"), "SDL.Null")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("null"), "SDL.Null"), "Null")(TParseTree("", false,[], s));
        }
    }
    static string Null(GetName g)
    {
        return "SDL.Null";
    }

    static TParseTree Base64(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("+"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("/")), pegged.peg.discard!(Spacing)))), pegged.peg.discard!(pegged.peg.literal!("]"))), "SDL.Base64")(p);
        }
        else
        {
            if (auto m = tuple(`Base64`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("+"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("/")), pegged.peg.discard!(Spacing)))), pegged.peg.discard!(pegged.peg.literal!("]"))), "SDL.Base64"), "Base64")(p);
                memo[tuple(`Base64`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Base64(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("+"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("/")), pegged.peg.discard!(Spacing)))), pegged.peg.discard!(pegged.peg.literal!("]"))), "SDL.Base64")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.literal!("[")), pegged.peg.fuse!(pegged.peg.zeroOrMore!(pegged.peg.or!(pegged.peg.or!(pegged.peg.charRange!('a', 'z'), pegged.peg.charRange!('A', 'Z'), pegged.peg.literal!("+"), pegged.peg.charRange!('0', '9'), pegged.peg.literal!("/")), pegged.peg.discard!(Spacing)))), pegged.peg.discard!(pegged.peg.literal!("]"))), "SDL.Base64"), "Base64")(TParseTree("", false,[], s));
        }
    }
    static string Base64(GetName g)
    {
        return "SDL.Base64";
    }

    static TParseTree UTC(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("-UTC"), "SDL.UTC")(p);
        }
        else
        {
            if (auto m = tuple(`UTC`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.literal!("-UTC"), "SDL.UTC"), "UTC")(p);
                memo[tuple(`UTC`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree UTC(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.literal!("-UTC"), "SDL.UTC")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.literal!("-UTC"), "SDL.UTC"), "UTC")(TParseTree("", false,[], s));
        }
    }
    static string UTC(GetName g)
    {
        return "SDL.UTC";
    }

    static TParseTree Digit(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "SDL.Digit")(p);
        }
        else
        {
            if (auto m = tuple(`Digit`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "SDL.Digit"), "Digit")(p);
                memo[tuple(`Digit`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Digit(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "SDL.Digit")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.charRange!('0', '9'), "SDL.Digit"), "Digit")(TParseTree("", false,[], s));
        }
    }
    static string Digit(GetName g)
    {
        return "SDL.Digit";
    }

    static TParseTree Char(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any), pegged.peg.and!(backslash, doublequote)), "SDL.Char")(p);
        }
        else
        {
            if (auto m = tuple(`Char`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any), pegged.peg.and!(backslash, doublequote)), "SDL.Char"), "Char")(p);
                memo[tuple(`Char`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Char(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any), pegged.peg.and!(backslash, doublequote)), "SDL.Char")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(pegged.peg.and!(pegged.peg.negLookahead!(doublequote), pegged.peg.negLookahead!(endOfLine), pegged.peg.any), pegged.peg.and!(backslash, doublequote)), "SDL.Char"), "Char")(TParseTree("", false,[], s));
        }
    }
    static string Char(GetName g)
    {
        return "SDL.Char";
    }

    static TParseTree Spacing(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(BlockComment, pegged.peg.and!(backslash, endOfLine), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.keywords!(" ", "\t")))), "SDL.Spacing")(p);
        }
        else
        {
            if (auto m = tuple(`Spacing`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.or!(BlockComment, pegged.peg.and!(backslash, endOfLine), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.keywords!(" ", "\t")))), "SDL.Spacing"), "Spacing")(p);
                memo[tuple(`Spacing`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree Spacing(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.or!(BlockComment, pegged.peg.and!(backslash, endOfLine), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.keywords!(" ", "\t")))), "SDL.Spacing")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.or!(BlockComment, pegged.peg.and!(backslash, endOfLine), pegged.peg.discard!(pegged.peg.zeroOrMore!(pegged.peg.keywords!(" ", "\t")))), "SDL.Spacing"), "Spacing")(TParseTree("", false,[], s));
        }
    }
    static string Spacing(GetName g)
    {
        return "SDL.Spacing";
    }

    static TParseTree EOL(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(endOfLine, LineComment)), "SDL.EOL")(p);
        }
        else
        {
            if (auto m = tuple(`EOL`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(endOfLine, LineComment)), "SDL.EOL"), "EOL")(p);
                memo[tuple(`EOL`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree EOL(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(endOfLine, LineComment)), "SDL.EOL")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.discard!(pegged.peg.or!(endOfLine, LineComment)), "SDL.EOL"), "EOL")(TParseTree("", false,[], s));
        }
    }
    static string EOL(GetName g)
    {
        return "SDL.EOL";
    }

    static TParseTree LineComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!("//", "--", "#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "SDL.LineComment")(p);
        }
        else
        {
            if (auto m = tuple(`LineComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!("//", "--", "#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "SDL.LineComment"), "LineComment")(p);
                memo[tuple(`LineComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree LineComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!("//", "--", "#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "SDL.LineComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.discard!(pegged.peg.keywords!("//", "--", "#")), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(endOfLine), pegged.peg.any)), endOfLine), "SDL.LineComment"), "LineComment")(TParseTree("", false,[], s));
        }
    }
    static string LineComment(GetName g)
    {
        return "SDL.LineComment";
    }

    static TParseTree BlockComment(TParseTree p)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "SDL.BlockComment")(p);
        }
        else
        {
            if (auto m = tuple(`BlockComment`, p.end) in memo)
                return *m;
            else
            {
                TParseTree result = hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "SDL.BlockComment"), "BlockComment")(p);
                memo[tuple(`BlockComment`, p.end)] = result;
                return result;
            }
        }
    }

    static TParseTree BlockComment(string s)
    {
        if(__ctfe)
        {
            return         pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "SDL.BlockComment")(TParseTree("", false,[], s));
        }
        else
        {
            forgetMemo();
            return hooked!(pegged.peg.defined!(pegged.peg.and!(pegged.peg.literal!("/*"), pegged.peg.zeroOrMore!(pegged.peg.and!(pegged.peg.negLookahead!(pegged.peg.literal!("*/")), pegged.peg.any)), pegged.peg.literal!("*/")), "SDL.BlockComment"), "BlockComment")(TParseTree("", false,[], s));
        }
    }
    static string BlockComment(GetName g)
    {
        return "SDL.BlockComment";
    }

    static TParseTree opCall(TParseTree p)
    {
        TParseTree result = decimateTree(TagTree(p));
        result.children = [result];
        result.name = "SDL";
        return result;
    }

    static TParseTree opCall(string input)
    {
        if(__ctfe)
        {
            return SDL(TParseTree(``, false, [], input, 0, 0));
        }
        else
        {
            forgetMemo();
            return SDL(TParseTree(``, false, [], input, 0, 0));
        }
    }
    static string opCall(GetName g)
    {
        return "SDL";
    }


    static void forgetMemo()
    {
        memo = null;
    }
    }
}

alias GenericSDL!(ParseTree).SDL SDL;

