# JSON Parser

This parser uses Applicative and the newtype, Parser, to parse JSON.

It either succeeds or fails, returning Nothing.

Note that the newtype, ```Parser```, as well as some of the other code within **SExpr.hs** and 
**AParser.hs** comes from Professor Brent Yorgey's U of Penn [class](http://www.cis.upenn.edu/~cis194/spring13/).

# How to Run

```
Prelude> :l JsonParser.hs 
[1 of 4] Compiling AParser          ( AParser.hs, interpreted )
[2 of 4] Compiling Model            ( Model.hs, interpreted )
[3 of 4] Compiling SExpr            ( SExpr.hs, interpreted )
[4 of 4] Compiling JsonParser       ( JsonParser.hs, interpreted )
Ok, modules loaded: JsonParser, SExpr, Model, AParser.
*JsonParser> runParser parseJson "[1,2,3]"
Just (JArray (Arr [Num 1.0,Num 2.0,Num 3.0]),"")

*JsonParser> runParser parseJson " { \"foo\" : \"bar\" } "
Just (JObject "foo" (fromList [S "bar"])," ")

*JsonParser> runParser parseJson " { \"foo\" : [\"bar\", true, null] } "
Just (JObject "foo" (fromList [S "bar",B True,N Null])," ")

*JsonParser> runParser parseJson " { \"foo\" : whoops "
Nothing

*JsonParser> runParser parseJson " { \"foo\" : [1,2,\"\\\" with escaped quote \\\" \"] } "
Just (JObject "foo" (fromList [Num 1.0,Num 2.0,S "\" with escaped quote \" "])," ")

```

# TODO

* Support all escaped characters
* Re-factor to return error information upon a parse error
* Test it (besides the few examples I've used so far)