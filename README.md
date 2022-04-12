An applicative Json parser written in Haskell

## Preface
Watch Tsoding's [video](https://www.youtube.com/watch?v=N9RUqGYuGfw) on the subject.

## Usage
Compile using `ghc`:
```
ghc Main.hs
```

Run alongside a json file to get the parsed `Show` haskell object:
```
./Main test.json
Just ("\n",JsonObject (fromList [("address",JsonArray [JsonString "62 Dingle Avenue",JsonString "GUX XX3"]),("age",JsonNumber 17),("name",JsonString "John Doe"),("qualifications",JsonObject (fromList [("C.S",JsonNumber 9),("Maths",JsonNumber 9)]))]))
```
