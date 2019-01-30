## Chapter 6. Using Typeclasses


Typeclasses are among the most powerful features in Haskell. They allow you to define generic interfaces that provide a common feature set over a wide variety of types. Typeclasses are at the heart of some basic language features such as equality testing and numeric operators. Before we talk about what exactly typeclasses are, though, we'd like to explain the need for them. 

### The need for typeclasses

Let's imagine that for some unfathomable reason, the designers of the Haskell language neglected to implement the equality test `==`. Once you got over your shock at hearing this, you resolved to implement your own equality tests. Your application consisted of a simple `Color` type, and so your first equality test is for this type. Your first attempt might look like this: 

```haskell
-- arquivo: src/Ch06.hs
data Color = Red | Green | Blue

colorEq :: Color -> Color -> Bool
colorEq Red   Red   = True
colorEq Green Green = True
colorEq Blue  Blue  = True
colorEq _     _     = False
```

You can test this with **ghci**: 

```
$ stack ghci src/Ch06.hs
ghci> colorEq Red Red
True
ghci> colorEq Red Green
False
```
Now, let's say that you want to add an equality test for `String`s. Since a Haskell `String` is a list of characters, we can write a simple function to perform that test. For simplicity, we cheat a bit and use the `==` operator here to illustrate. 

```haskell
-- arquivo: src/Ch06.hs
stringEq :: [Char] -> [Char] -> Bool
stringEq [] [] = True
stringEq (x:xs) (y:ys) = x == y && stringEq xs ys
stringEq _ _ = False
```

You should now be able to see a problem: we have to use a function with a different name for every different type that we want to be able to compare. That's inefficient and annoying. It's much more convenient to be able to just use `==` to compare anything. It may also be useful to write generic functions such as `/=` that could be implemented in terms of `==`, and valid for almost anything. By having a generic function that can compare anything, we can also make our code generic: if a piece of code only needs to compare things, then it ought to be able to accept any data type that the compiler knows how to compare. And, what's more, if new data types are added later, the existing code shouldn't have to be modified. 

Haskell's typeclasses are designed to address all of these things. 

### What are typeclasses?


Typeclasses define a set of functions that can have different implementations depending on the type of data they are given. Typeclasses may look like the objects of object-oriented programming, but they are truly quite different.

Let's use typeclasses to solve our equality dilemma from earlier in the chapter. To begin with, we must define the typeclass itself. We want a function that takes two parameters, both the same type, and returns a `Bool` indicating whether or not they are equal. We don't care what that type is, but we just want two items of that type. Here's our first definition of a typeclass: 

```haskell
-- arquivo: src/Ch06.hs
class BasicEq a where
    isEqual :: a -> a -> Bool
```

This says that we are declaring a typeclass named `BasicEq`, and we'll refer to instance types with the letter `a`. An instance type of this typeclass is any type that implements the functions defined in the typeclass. This typeclass defines one function. That function takes two parameters—both corresponding to instance types—and returns a `Bool`. 

>**NOTA**
>When is a class not a class?
>
>The keywoard to define a typeclass in Haskell is `class`. Unfortunately, this may be confusing for those of you coming from an object-oriented background, as we are not really defining the same thing. 

On the first line, the name of the parameter `a` was chosen arbitrarily. We could have used any name. The key is that, when you list the types of your functions, you must use that name to refer to instance types. 

Let's look at this in **ghci**. Recall that you can type **:type** in **ghci** to have it show you the type of something. Let's see what it says about `isEqual`: 

```
Ch06> :type isEqual
isEqual :: (BasicEq a) => a -> a -> Bool
``` 

You can read that this way: "For all types `a`, so long as `a` is an instance of `BasicEq`, `isEqual` takes two parameters of type `a` and returns a `Bool`". Let's take a quick look at defining `isEqual` for a particular type. 

```haskell
-- arquivo: src/Ch06.hs
instance BasicEq Bool where
    isEqual True  True  = True
    isEqual False False = True
    isEqual _     _     = False
```
You can also use **ghci** to verify that we can now use `isEqual` on `Bool`s, but not on any other type: 

```
Ch06> isEqual False False
True
Ch06> isEqual False True
False
<interactive>:5:1: error:
    • No instance for (BasicEq [Char]) arising from a use of ‘isEqual’
    • In the expression: isEqual "Hi" "Hi"
      In an equation for ‘it’: it = isEqual "Hi" "Hi"
``` 

Notice that when we tried to compare two strings, **ghci** noticed that we hadn't provided an instance of `BasicEq` for `String`. It therefore didn't know how to compare a `String`, and suggested that we could fix the problem by defining an instance of `BasicEq` for `[Char]`, which is the same as `String`. 

We'll go into more detail on defining instances in [the section called “Declaring typeclass instances”](using-typeclasses.html#typeclasses.instances "Declaring typeclass instances"). First, though, let's continue to look at ways to define typeclasses. In this example, a not-equal-to function might be useful. Here's what we might say to define a typeclass with two functions: 

```haskell
-- arquivo: src/Ch06.hs
class BasicEq2 a where
    isEqual2    :: a -> a -> Bool
    isNotEqual2 :: a -> a -> Bool
```


Someone providing an instance of `BasicEq2` will be required to define two functions: `isEqual2` and `isNotEqual2`. 

While our definition of `BasicEq2` is fine, it seems that we're making extra work for ourselves. Logically speaking, if we know what `isEqual` or `isNotEqual` would return, we know how to figure out what the other function would return, for all types. Rather than making users of the typeclass define both functions for all types, we can provide default implementations for them. Then, users will only have to implement one function. \[[12](#ftn.id603171)\] Here's an example that shows how to do this. 

```haskell
-- arquivo: src/Ch06.hs
class BasicEq3 a where
    isEqual3 :: a -> a -> Bool
    isEqual3 x y = not (isNotEqual3 x y)

    isNotEqual3 :: a -> a -> Bool
    isNotEqual3 x y = not (isEqual3 x y)
```

People implementing this class must provide an implementation of at least one function. They can implement both if they wish, but they will not be required to. While we did provide defaults for both functions, each function depends on the presence of the other to calculate an answer. If we don't specify at least one, the resulting code would be an endless loop. Therefore, at least one function must always be implemented. 

With `BasicEq3`, we have provided a class that does very much the same thing as Haskell's built-in `==` and `/=` operators. In fact, these operators are defined by a typeclass that looks almost identical to `BasicEq3`. The Haskell 98 Report defines a typeclass that implements equality comparison. Here is the code for the built-in `Eq` typeclass. Note how similar it is to our `BasicEq3` typeclass. 

```haskell
-- definido em Prelude.hs
class  Eq a  where
    (==), (/=) :: a -> a -> Bool

       -- Minimal complete definition:
       --     (==) or (/=)
    x /= y     =  not (x == y)
    x == y     =  not (x /= y)
 ```
 
#### Declaring typeclass instances

Now that you know how to define typeclasses, it's time to learn how to define instances of typeclasses. Recall that types are made instances of a particular typeclass by implementing the functions necessary for that typeclass. 

Recall our attempt to create a test for equality over a `Color` type back in [the section called “The need for typeclasses”](using-typeclasses.html#typeclasses.need "The need for typeclasses"). Now let's see how we could make that same `Color` type a member of the `BasicEq3` class. 

```haskell
-- arquivo: src/Ch06.hs
instance BasicEq3 Color where
    isEqual3 Red Red = True
    isEqual3 Green Green = True
    isEqual3 Blue Blue = True
    isEqual3 _ _ = False
```

Notice that we provide essentially the same function as we used back in [the section called “The need for typeclasses”](using-typeclasses.html#typeclasses.need "The need for typeclasses"). In fact, the implementation is identical. However, in this case, we can use `isEqual3` on _any_ type that we declare is an instance of `BasicEq3`, not just this one color type. We could define equality tests for anything from numbers to graphics using the same basic pattern. In fact, as you will see in [the section called “Equality, Ordering, and Comparisons”](using-typeclasses.html#typeclasses.wellknown.equality "Equality, Ordering, and Comparisons"), this is exactly how you can make Haskell's `==` operator work for your own custom types. 

Note also that the `BasicEq3` class defined both `isEqual3` and `isNotEqual3`, but we implemented only one of them in the `Color` instance. That's because of the default implementation contained in `BasicEq3`. Since we didn't explicitly define `isNotEqual3`, the compiler automatically uses the default implementation given in the `BasicEq3` declaration. 

### Important Built-In Typeclasses

Now that we've discussed defining your own typeclasses and making your types instances of typeclasses, it's time to introduce you to typeclasses that are a standard part of the Haskell Prelude. As we mentioned at the beginning of this chapter, typeclasses are at the core of some important aspects of the language. We'll cover the most common ones here. For more details, the Haskell library reference is a good resource. It will give you a description of the typeclasses, and usually also will tell you which functions you must implement to have a complete definition. 

####Show

The `Show` typeclass is used to convert values to `String`s. It is perhaps most commonly used to convert numbers to `String`s, but it is defined for so many types that it can be used to convert quite a bit more. If you have defined your own types, making them instances of `Show` will make it easy to display them in **ghci** or print them out in programs. 

```
*Ch06> :info show
class Show a where
  ...
  show :: a -> String
  ...
        -- Defined in ‘GHC.Show’
```

The most important function of `Show` is `show`. It takes one argument: the data to convert. It returns a `String` representing that data. **ghci** reports the type of `show` like this: 

```
ghci>:type show
show :: (Show a) => a -> String
```

Let's look at some examples of converting values to strings: 

```
ghci> show 1
"1"
ghci> show [1, 2, 3]
"[1,2,3]"
ghci> show (1, 2)
"(1,2)" 
```

Remember that **ghci** displays results as they would be entered into a Haskell program. So the expression `show 1` returns a single-character string containing the digit `1`. That is, the quotes are not part of the string itself. We can make that clear by using `putStrLn`: 

```
ghci> putStrLn (show 1)
1
ghci> putStrLn (show [1,2,3])
[1,2,3]
```

You can also use `show` on `String`s: 

```
ghci> show "Hello!"
"\"Hello!\""
ghci> putStrLn (show "Hello!")
"Hello!"
ghci> show ['H', 'i']
"\"Hi\""
ghci> putStrLn (show "Hi")
"Hi"
ghci> show "Hi, \"Jane\""
"\"Hi, \\\"Jane\\\"\""
ghci> putStrLn (show "Hi, \"Jane\"")
"Hi, \"Jane\""
```

Running `show` on `String`s can be confusing. Since `show` generates a result that is suitable for a Haskell literal, `show` adds quotes and escaping suitable for inclusion in a Haskell program. **ghci** also uses `show` to display results, so quotes and escaping get added twice. Using `putStrLn` can help make this difference clear. 

You can define a `Show` instance for your own types easily. Here's an example: 

```haskell
-- arquivo: src/Ch06.hs
instance Show Color where
    show Red   = "Red"
    show Green = "Green"
    show Blue  = "Blue"
```

This example defines an instance of `Show` for our type `Color` (see [the section called “The need for typeclasses”](using-typeclasses.html#typeclasses.need "The need for typeclasses")). The implementation is simple: we define a function `show` and that's all that's needed. 


> The Show typeclass
>
>`Show` is usually used to define a `String` representation for data that is useful for a machine to parse back with `Read`. Haskell programmers generally write custom functions to format data in pretty ways for displaying to end users, if this representation would be different than expected via `Show`. 

#### Read

The `Read` typeclass is essentially the opposite of `Show`: it defines functions that will take a `String`, parse it, and return data in any type that is a member of `Read`. The most useful function in `Read` is `read`. You can ask **ghci** for its type like this: 

```
:type read
read :: (Read a) => String -> a
```


Here's an example illustrating the use of `read` and `show`: 

```haskell
-- arquivo: src/Ch06.hs
main = do
        putStrLn "Please enter a Double:"
        inpStr <- getLine
        let inpDouble = (read inpStr)::Double
        putStrLn ("Twice " ++ show inpDouble ++ " is " ++ show (inpDouble * 2))
```

This is a simple example of `read` and `show` together. Notice that we gave an explicit type of `Double` when processing the `read`. That's because `read` returns a value of type `Read a => a` and `show` expects a value of type `Show a => a`. There are many types that have instances defined for both `Read` and `Show`. Without knowing a specific type, the compiler must guess from these many types which one is needed. In situations like this, it may often choose `Integer`. If we wanted to accept floating-point input, this wouldn't work, so we provided an explicit type. 


> A note about defaulting

> In most cases, if the explicit `Double` type annotation were omitted, the compiler would refuse to guess a common type and simply give an error. The fact that it could default to `Integer` here is a special case arising from the fact that the literal `2` is treated as an `Integer` unless a different type of expected for it. 

You can see the same effect at work if you try to use `read` on the **ghci** command line. **ghci** internally uses `show` to display results, meaning that you can hit this ambiguous typing problem there as well. You'll need to explicitly give types for your `read` results in **ghci** as shown here: 

```
*Ch06> read "5"
*** Exception: Prelude.read: no parse
*Ch06> read "5" :: Int
5
*Ch06> read "5" :: Double
5.0
```

Recall the type of `read`: `(Read a) => String -> a`. The `a` here is the type of each instance of `Read`. Which particular parsing function is called depends upon the type that is expected from the return value of `read`. Let's see how that works: 

```
*Ch06> read "5.0" :: Double
5.0
*Ch06> read "5.0" :: Int
*** Exception: Prelude.read: no parse
```

Notice the error when trying to parse `5.0` as an `Integer`. The interpreter selected a different instance of `Read` when the return value was expected to be `Integer` than it did when a `Double` was expected. The `Integer` parser doesn't accept decimal points, and caused an exception to be raised. 

The `Read` class provides for some fairly complicated parsers. You can define a simple parser by providing an implementation for the `readsPrec` function. Your implementation can return a list containing exactly one tuple on a successful parse, or an empty list on an unsuccessful parse. Here's an example implementation: 

```haskell
-- arquivo: src/Ch06.hs
instance Read Color where
    -- readsPrec is the main function for parsing input
    readsPrec _ value = 
        -- We pass tryParse a list of pairs.  Each pair has a string
        -- and the desired return value.  tryParse will try to match
        -- the input to one of these strings.
        tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where tryParse [] = []    -- If there is nothing left to try, fail
              tryParse ((attempt, result):xs) =
                      -- Compare the start of the string to be parsed to the
                      -- text we are looking for.
                      if (take (length attempt) value) == attempt
                         -- If we have a match, return the result and the
                         -- remaining input
                         then [(result, drop (length attempt) value)]
                         -- If we don't have a match, try the next pair
                         -- in the list of attempts.
                         else tryParse xs
```

This example handles the known cases for the three colors. It returns an empty list (resulting in a "no parse" message) for others. The function is supposed to return the part of the input that was not parsed, so that the system can integrate the parsing of different types together. Here's an example of using this new instance of `Read`: 

```
ghci> (read "Red")::Color
Red
ghci> (read "Green")::Color
Green
ghci> (read "Blue")::Color
Blue
ghci> (read "[Red]")::[Color]
[Red]
ghci> (read "[Red,Red,Blue]")::[Color]
[Red,Red,Blue]
ghci> (read "[Red, Red, Blue]")::[Color]
*** Exception: Prelude.read: no parse
```


Notice the error on the final attempt. That's because our parser is not smart enough to handle leading spaces yet. If we modified it to accept leading spaces, that attempt would work. You could rectify this by modifying your `Read` instance to discard any leading spaces, which is common practice in Haskell programs. 



> Read is not widely used
>
>While it is possible to build sophisticated parsers using the `Read` typeclass, many people find it easier to do so using Parsec, and rely on `Read` only for simpler tasks. Parsec is covered in detail in [Chapter 16, _Using Parsec_](using-parsec.html "Chapter 16. Using Parsec"). 

#### Serialization with Read and Show

You may often have a data structure in memory that you need to store on disk for later retrieval or to send across the network. The process of converting data in memory to a flat series of bits for storage is called _serialization_. 

It turns out that `read` and `show` make excellent tools for serialization. `show` produces output that is both human-readable and machine-readable. Most `show` output is also syntactically-valid Haskell, though it is up to people that write `Show` instances to make it so. 

>Parsing large strings
>
>String handling in Haskell is normally lazy, so `read` and `show` can be used on quite large data structures without incident. The built-in `read` and `show` instances in Haskell are efficient and implemented in pure Haskell. For information on how to handle parsing exceptions, refer to [Chapter 19, _Error handling_](error-handling.html "Chapter 19. Error handling"). 

Let's try it out in **ghci**: 

```
ghci> let d1 = [Just 5, Nothing, Nothing, Just 8, Just 9]::[Maybe Int]
ghci> putStrLn (show d1)
[Just 5,Nothing,Nothing,Just 8,Just 9]
ghci> writeFile "teste.txt" (show d1)
```


First, we assign `d1` to be a list. Next, we print out the result of `show d1` so we can see what it generates. Then, we write the result of `show d1` to a file named `test`. 


```
*Ch06> input <- readFile "teste.txt"
*Ch06> input
"[Just 5,Nothing,Nothing,Just 8,Just 9]"
*Ch06> let d2 = read input
*Ch06> d2
*** Exception: Prelude.read: no parse
*Ch06> let d2 = read input :: [Maybe Int]
*Ch06> d2
[Just 5,Nothing,Nothing,Just 8,Just 9]
*Ch06> d1
[Just 5,Nothing,Nothing,Just 8,Just 9]
*Ch06>
```


First, we ask Haskell to read the file back.\[[13](#ftn.id604799)\] Then, we try to assign the result of `read input` to `d2`. That generates an error. The reason is that the interpreter doesn't know what type `d2` is meant to be, so it doesn't know how to parse the input. If we give it an explicit type, it works, and we can verify that the two sets of data are equal. 

Since so many different types are instances of `Read` and `Show` by default (and others can be made instances easily; see [the section called “Automatic Derivation”](using-typeclasses.html#typeclasses.auto.derivation "Automatic Derivation")), you can use it for some really complex data structures. Here are a few examples of slightly more complex data structures: 


```
ghci> putStrLn $ show [("hi", 1), ("there", 3)]
[("hi",1),("there",3)]
ghci> putStrLn $ show [[1, 2, 3], [], [4, 0, 1], [], [503]]
[[1,2,3],[],[4,0,1],[],[503]]
ghci> putStrLn $ show [Left 5, Right "three", Left 0, Right "nine"]
[Left 5,Right "three",Left 0,Right "nine"]
ghci> putStrLn $ show [Left 0, Right [1, 2, 3], Left 5, Right []]
[Left 0,Right [1,2,3],Left 5,Right []]
```


### Numeric Types


Haskell has a powerful set of numeric types. You can use everything from fast 32-bit or 64-bit integers to arbitrary-precision rational numbers. You probably know that operators such as `+` can work with just about all of these. This feature is implemented using typeclasses. As a side benefit, it allows you to define your own numeric types and make them first-class citizens in Haskell. 

Let's begin our discussion of the typeclasses surrounding numeric types with an examination of the types themselves. [Table 6.1, “Selected Numeric Types”](using-typeclasses.html#numerictypes.summary "Table 6.1. Selected Numeric Types") describes the most commonly-used numeric types in Haskell. Note that there are also many more numeric types available for specific purposes such as interfacing to C. 

**Table 6.1. Selected Numeric Types**

| Type     | Description                                                                                 |
|----------|---------------------------------------------------------------------------------------------|
| Double   | Double-precision floating point. A common choice for floating-point data.                   |
| Float    | Single-precision floating point. Often used when interfacing with C.                        |
| Int      | Fixed-precision signed integer; minimum range [-2^29..2^29-1]. Commonly used.               |
| Int8     | 8-bit signed integer                                                                        |
| Int16    | 16-bit signed integer                                                                       |
| Int32    | 32-bit signed integer                                                                       |
| Int64    | 64-bit signed integer                                                                       |
| Integer  | Arbitrary-precision signed integer; range limited only by machine resources. Commonly used. |
| Rational | Arbitrary-precision rational numbers. Stored as a ratio of two Integers.                    |
| Word     | Fixed-precision unsigned integer; storage size same as Int                                  |
| Word8    | 8-bit unsigned integer                                                                      |
| Word16   | 16-bit unsigned integer                                                                     |
| Word32   | 32-bit unsigned integer                                                                     |
| Word64   | 64-bit unsigned integer                                                                     |


These are quite a few different numeric types. There are some operations, such as addition, that work with all of them. There are others, such as `asin`, that only apply to floating-point types. [Table 6.2, “Selected Numeric Functions and Constants”](using-typeclasses.html#numerictypes.funcs "Table 6.2. Selected Numeric Functions and Constants") summarizes the different functions that operate on numeric types, and [Table 6.3, “Typeclass Instances for Numeric Types”](using-typeclasses.html#numerictypes.typeclasses "Table 6.3. Typeclass Instances for Numeric Types") matches the types with their respective typeclasses. As you read that table, keep in mind that Haskell operators are just functions: you can say either `(+) 2 3` or `2 + 3` with the same result. By convention, when referring to an operator as a function, it is written in parenthesis as seen in this table. 

**Table 6.2. Selected Numeric Functions and Constants**

| Item           | Type                                      | Module     | Description                                                                          |
|----------------|-------------------------------------------|------------|--------------------------------------------------------------------------------------|
| (+)            | Num a => a -> a -> a                      | Prelude    | Addition                                                                             |
| (-)            | Num a => a -> a -> a                      | Prelude    | Subtraction                                                                          |
| (*)            | Num a => a -> a -> a                      | Prelude    | Multiplication                                                                       |
| (/)            | Fractional a => a -> a -> a               | Prelude    | Fractional division                                                                  |
| (**)           | Floating a => a -> a -> a                 | Prelude    | Raise to the power of                                                                |
| (^)            | (Num a, Integral b) => a -> b -> a        | Prelude    | Raise a number to a non-negative, integral power                                     |
| (^^)           | (Fractional a, Integral b) => a -> b -> a | Prelude    | Raise a fractional number to any integral power                                      |
| (%)            | Integral a => a -> a -> Ratio a           | Data.Ratio | Ratio composition                                                                    |
| (.&.)          | Bits a => a -> a -> a                     | Data.Bits  | Bitwise and                                                                          |
| (.|.)          | Bits a => a -> a -> a                     | Data.Bits  | Bitwise or                                                                           |
| abs            | Num a => a -> a                           | Prelude    | Absolute value                                                                       |
| approxRational | RealFrac a => a -> a -> Rational          | Data.Ratio | Approximate rational composition based on fractional numerators and denominators     |
| cos            | Floating a => a -> a                      | Prelude    | Cosine. Also provided are acos, cosh, and acosh, with the same type.                 |
| div            | Integral a => a -> a -> a                 | Prelude    | Integer division always truncated down; see alsoquot                                 |
| fromInteger    | Num a => Integer -> a                     | Prelude    | Conversion from an Integer to any numeric type                                       |
| fromIntegral   | (Integral a, Num b) => a -> b             | Prelude    | More general conversion from any Integral to any numeric type                        |
| fromRational   | Fractional a => Rational -> a             | Prelude    | Conversion from a Rational. May be lossy.                                            |
| log            | Floating a => a -> a                      | Prelude    | Natural logarithm                                                                    |
| logBase        | Floating a => a -> a -> a                 | Prelude    | Log with explicit base                                                               |
| maxBound       | Bounded a => a                            | Prelude    | The maximum value of a bounded type                                                  |
| minBound       | Bounded a => a                            | Prelude    | The minimum value of a bounded type                                                  |
| mod            | Integral a => a -> a -> a                 | Prelude    | Integer modulus                                                                      |
| pi             | Floating a => a                           | Prelude    | Mathematical constant pi                                                             |
| quot           | Integral a => a -> a -> a                 | Prelude    | Integer division; fractional part of quotient truncated towards zero                 |
| recip          | Fractional a => a -> a                    | Prelude    | Reciprocal                                                                           |
| rem            | Integral a => a -> a -> a                 | Prelude    | Remainder of integer division                                                        |
| round          | (RealFrac a, Integral b) => a -> b        | Prelude    | Rounds to nearest integer                                                            |
| shift          | Bits a => a -> Int -> a                   | Bits       | Shift left by the specified number of bits, which may be negative for a right shift. |
| sin            | Floating a => a -> a                      | Prelude    | Sine. Also provided are asin, sinh, and asinh, with the same type.                   |
| sqrt           | Floating a => a -> a                      | Prelude    | Square root                                                                          |
| tan            | Floating a => a -> a                      | Prelude    | Tangent. Also provided are atan, tanh, and atanh, with the same type.                |
| toInteger      | Integral a => a -> Integer                | Prelude    | Convert any Integral to an Integer                                                   |
| toRational     | Real a => a -> Rational                   | Prelude    | Convert losslessly to Rational                                                       |
| truncate       | (RealFrac a, Integral b) => a -> b        | Prelude    | Truncates number towards zero                                                        |
| xor            | Bits a => a -> a -> a                     | Data.Bits  | Bitwise exclusive or                                                                 |


**Table 6.3. Typeclass Instances for Numeric Types**

| Type                  | Bits | Bounded | Floating | Fractional | Integral | Num | Real | RealFrac |
|-----------------------|------|---------|----------|------------|----------|-----|------|----------|
| Double                |      |         | X        | X          |          | X   | X    | X        |
| Float                 |      |         | X        | X          |          | X   | X    | X        |
| Int                   | X    | X       |          |            | X        | X   | X    |          |
| Int16                 | X    | X       |          |            | X        | X   | X    |          |
| Int32                 | X    | X       |          |            | X        | X   | X    |          |
| Int64                 | X    | X       |          |            | X        | X   | X    |          |
| Integer               | X    |         |          |            | X        | X   | X    |          |
| Rational or any Ratio |      |         |          | X          |          | X   | X    | X        |
| Word                  | X    | X       |          |            | X        | X   | X    |          |
| Word16                | X    | X       |          |            | X        | X   | X    |          |
| Word32                | X    | X       |          |            | X        | X   | X    |          |
| Word64                | X    | X       |          |            | X        | X   | X    |          |


  

Converting between numeric types is another common need. [Table 6.2, “Selected Numeric Functions and Constants”](using-typeclasses.html#numerictypes.funcs "Table 6.2. Selected Numeric Functions and Constants") listed many functions that can be used for conversion. However, it is not always obvious how to apply them to convert between two arbitrary types. To help you out, [Table 6.4, “Conversion Between Numeric Types”](using-typeclasses.html#numerictypes.conversion "Table 6.4. Conversion Between Numeric Types") provides information on converting between different types. 

**Table 6.4. Conversion Between Numeric Types**

| Source Type   | Destination Type          |              |              |              |
|---------------|---------------------------|--------------|--------------|--------------|
|               | Int, Word                 | Integer      | Rational     |              |
| Double, Float | fromRational . toRational | truncate *   | truncate *   | toRational   |
| Int, Word     | fromIntegral              | fromIntegral | fromIntegral | fromIntegral |
| Integer       | fromIntegral              | fromIntegral | N/A          | fromIntegral |
| Rational      | fromRational              | truncate *   | truncate *   | N/A          |
|               |                           |              |              |              |
  

\* Instead of `truncate`, you could also use `round`, `ceiling`, or `floor`. 

For an extended example demonstrating the use of these numeric typeclasses, see [the section called “Extended example: Numeric Types”](data-structures.html#data.num "Extended example: Numeric Types"). 

### Equality, Ordering, and Comparisons

We've already talked about the arithmetic operators such as `+` that can be used for all sorts of different numbers. But there are some even more widely-applied operators in Haskell. The most obvious, of course, are the equality tests: `==` and `/=`. These operators are defined in the `Eq` class. 

There are also comparison operators such as `>=` and `<=`. These are declared by the `Ord` typeclass. These are in a separate typeclass because there are some types, such as `Handle`, where an equality test makes sense, but there is no way to express a particular ordering. Anything that is an instance of `Ord` can be sorted by `Data.List.sort`. 

Almost all Haskell types are instances of `Eq`, and nearly as many are instances of `Ord`. 


> Tip
>
> Sometimes, the ordering in `Ord` is arbitrary. For instance, for `Maybe`, `Nothing` sorts before `Just x`, but this was a somewhat arbitrary decision. 

#### Automatic Derivation


For many simple data types, the Haskell compiler can automatically derive instances of `Read`, `Show`, `Bounded`, `Enum`, `Eq`, and `Ord` for us. This saves us the effort of having to manually write code to compare or display our own types. 

```haskell
data Color = Red | Green | Blue
     deriving (Read, Show, Eq, Ord)
```

> Which types can be automatically derived?
>
>The Haskell standard requires compilers to be able to automatically derive instances of these specific typeclasses. This automation is not available for other typeclasses. 

Let's take a look at how these derived instances work for us: 

```
ghci> show Red
"Red"
ghci> (read "Red")::Color
Red
ghci> (read "[Red,Red,Blue]")::[Color]
[Red,Red,Blue]
ghci> (read "[Red, Red, Blue]")::[Color]
[Red,Red,Blue]
ghci> Red == Red
True
ghci> Red == Blue
False
ghci> Data.List.sort [Blue,Green,Blue,Red]
[Red,Green,Blue,Blue]
ghci> Red < Blue
True
```


Notice that the sort order for `Color` was based on the order that the constructors were defined. 

Automatic derivation is not always possible. For instance, if you defined a type `data MyType = MyType (Int -> Bool)`, the compiler will not be able to derive an instance of `Show` because it doesn't know how to render a function. We will get a compilation error in such a situation. 

When we automatically derive an instance of some typeclass, the types that we refer to in our `data` declaration must also be instances of that typeclass (manually or automatically). 

```haskell
-- arquivo: src/Ch06.hs
data CannotShow = CannotShow
                
-- will not compile, since CannotShow is not an instance of Show
--data CannotDeriveShow = CannotDeriveShow CannotShow deriving (Show)

data OK = OK

instance Show OK where
    show _ = "OK"

data ThisWorks = ThisWorks OK
                 deriving (Show)

```

#### Typeclasses at work: making JSON easier to use

The JValue type that we introduced in [the section called “Representing JSON data in Haskell”](writing-a-library-working-with-json-data.html#library.jvalue "Representing JSON data in Haskell") is not especially easy to work with. Here is a truncated and tidied snippet of some real JSON data, produced by a well known search engine. 
```
{
  "query": "awkward squad haskell",
  "estimatedCount": 3920,
  "moreResults": true,
  "results":
  [{
    "title": "Simon Peyton Jones: papers",
    "snippet": "Tackling the awkward squad: monadic input/output ...",
    "url": "http://research.microsoft.com/~simonpj/papers/marktoberdorf/",
   },
   {
    "title": "Haskell for C Programmers | Lambda the Ultimate",
    "snippet": "... the best job of all the tutorials I've read ...",
    "url": "http://lambda-the-ultimate.org/node/724",
   }]
}
```
And here's a further slimmed down fragment of that data, represented in Haskell. 

https://github.com/profsergiocosta/hs2json-cap6


```haskell
-- arquivo: app/Main.hs
module Main (main) where

import SimpleJSON

result :: JValue
result = JObject [
  ("query", JString "awkward squad haskell"),
  ("estimatedCount", JNumber 3920),
  ("moreResults", JBool True),
  ("results", JArray [
     JObject [
      ("title", JString "Simon Peyton Jones: papers"),
      ("snippet", JString "Tackling the awkward ..."),
      ("url", JString "http://.../marktoberdorf/")
     ]])
  ]

main :: IO ()
main = do
    putStrLn (show result)
```

Because Haskell doesn't natively support lists that contain types of different value, we can't directly represent a JSON object that contains values of different types. Instead, we must wrap each value with a JValue constructor. This limits our flexibility: if we want to change the number `3920` to a string `"3,920"`, we must change the constructor that we use to wrap it from `JNumber` to `JString`. 

Haskell's typeclasses offer a tempting solution to this problem. 

```haskell
-- arquivo: src/SimpleJSON.hs
type JSONError = String

class JSON a where
    toJValue :: a -> JValue
    fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
    toJValue = id
    fromJValue = Right
```


Now, instead of applying a constructor like `JNumber` to a value to wrap it, we apply the `toJValue` function. If we change a value's type, the compiler will choose a suitable implementation of toJValue to use with it. 

We also provide a `fromJValue` function, which attempts to convert a JValue into a value of our desired type. 

#### More helpful errors

The return type of our `fromJValue` function uses the Either type. Like Maybe, this type is predefined for us, and we'll often use it to represent a computation that could fail. 

While Maybe is useful for this purpose, it gives us no information if a failure occurs: we literally have `Nothing`. The Either type has a similar structure, but instead of `Nothing`, the “something bad happened” constructor is named `Left`, and it takes a parameter. 

```haskell
-- definido em Prelude.hs
data Maybe a = Nothing
             | Just a
               deriving (Eq, Ord, Read, Show)

data Either a b = Left a
                | Right b
                  deriving (Eq, Ord, Read, Show)
```

Quite often, the type we use for the `a` parameter value is String, so we can provide a useful description if something goes wrong. To see how we use the Either type in practice, let's look at a simple instance of our typeclass. 

```haskell
-- arquivo: src/SimpleJSON.hs
instance JSON Bool where
    toJValue = JBool
    fromJValue (JBool b) = Right b
    fromJValue \_ = Left "not a JSON boolean"
```

#### Making an instance with a type synonym

The Haskell 98 standard does not allow us to write an instance of the following form, even though it seems perfectly reasonable. 

```haskell
-- arquivo: src/SimpleJSON.hs
instance JSON String where
    toJValue               = JString

    fromJValue (JString s) = Right s
    fromJValue _           = Left "not a JSON string"
```

Recall that String is a synonym for \[Char\], which in turn is the type \[a\] where Char is substituted for the type parameter `a`. According to Haskell 98's rules, we are not allowed to supply a type in place of a type parameter when we write an instance. In other words, it would be legal for us to write an instance for \[a\], but not for \[Char\]. [24 comments]

While GHC follows the Haskell 98 standard by default, we can relax this particular restriction by placing a specially formatted comment at the top of our source file. (rever essa explicacao para o haskell2010)

```haskell
-- arquivo: src/SimpleJSON.hs
{-# LANGUAGE FlexibleInstances #-}
```

This comment is a directive to the compiler, called a _pragma_, which tells it to enable a language extension. The `TypeSynonymInstances` language extension makes the above code legal. We'll encounter a few other language extensions in this chapter, and a handful more later in this book. 

#### Living in an open world


Haskell's typeclasses are intentionally designed to let us create new instances of a typeclass whenever we see fit. 

```haskell
-- arquivo: src/SimpleJSON.hs
doubleToJValue :: (Double -> a) -> JValue -> Either JSONError a
doubleToJValue f (JNumber v) = Right (f v)
doubleToJValue _ _ = Left "not a JSON number"

instance JSON Int where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Integer where
    toJValue = JNumber . realToFrac
    fromJValue = doubleToJValue round

instance JSON Double where
    toJValue = JNumber
    fromJValue = doubleToJValue id
```

Testando

```
*Main Prettify PrettyJSON PutJSON SimpleJSON> fromJValue (JNumber 12.6)
Right 13
*Main Prettify PrettyJSON PutJSON SimpleJSON> fromJValue (JNumber 12.6) :: Either JSONError Double
Right 12.6
*Main Prettify PrettyJSON PutJSON SimpleJSON> fromJValue (JNumber 12.6) :: Either JSONError Int
Int       Integer   Integral
*Main Prettify PrettyJSON PutJSON SimpleJSON> fromJValue (JNumber 12.6) :: Either JSONError Integer
Right 13
```



We can add new instances anywhere; they are not confined to the module where we define a typeclass. This feature of the typeclass system is referred to as its _open world assumption_. If we had a way to express a notion of “the following are the only instances of this typeclass that can exist”, we would have a _closed_ world. 

We would like to be able to turn a list into what JSON calls an array. We won't worry about implementation details just yet, so let's use `undefined` as the bodies of the instance's methods. 

```haskell
-- arquivo: src/SimpleJSON.hs
instance (JSON a) => JSON \[a\] where
    toJValue = undefined
    fromJValue = undefined
```

It would also be convenient if we could turn a list of name/value pairs into a JSON object. 

```haskell
-- arquivo: src/SimpleJSON.hs
instance (JSON a) => JSON [(String, a)] where
    toJValue = undefined
    fromJValue = undefined
```

#### When do overlapping instances cause problems?

If we put these definitions into a source file and load them into **ghci**, everything initially seems fine. 
However, once we try to _use_ the list-of-pairs instance, we run into trouble.
```
ghci> toJValue [("foo","bar")]

<interactive>:40:1: error:
    • Overlapping instances for JSON [([Char], [Char])]
        arising from a use of ‘toJValue’
      Matching instances:
        instance [safe] JSON a => JSON [(String, a)]
          -- Defined at /home/sergiosouzacosta/tmp/hs2json-cap6/hs2json/src/SimpleJSON.hs:94:10
        instance [safe] JSON a => JSON [a]
          -- Defined at /home/sergiosouzacosta/tmp/hs2json-cap6/hs2json/src/SimpleJSON.hs:90:10
    • In the expression: toJValue [("foo", "bar")]
      In an equation for ‘it’: it = toJValue [("foo", "bar")]

```

This problem of _overlapping instances_ is a consequence of Haskell's open world assumption. Here's a simpler example that makes it clearer what's going on. 

```haskell
-- arquivo: src/Ch06.hs
class Borked a where
    bork :: a -> String

instance Borked Int where
    bork = show

instance Borked (Int, Int) where
    bork (a, b) = bork a ++ ", " ++ bork b

instance (Borked a, Borked b) => Borked (a, b) where
    bork (a, b) = ">>" ++ bork a ++ " " ++ bork b ++ "<<"
```

We have two instances of the typeclass Borked for pairs: one for a pair of Ints and another for a pair of anything else that's Borked. 

Suppose that we want to `bork` a pair of Int values. To do so, the compiler must choose an instance to use. Because these instances are right next to each other, it may seem that it could simply choose the more specific instance. 

However, GHC is conservative by default, and insists that there must be only one possible instance that it can use. It will thus report an error if we try to use `bork`. 

![[Note]](/support/figs/note.png)

When do overlapping instances matter?

As we mentioned earlier, we can scatter instances of a typeclass across several modules. GHC does not complain about the mere existence of overlapping instances. Instead, it only complains when we try to use a method of the affected typeclass, when it is forced to make a decision about which instance to use. 

### Relaxing some restrictions on typeclasses

Normally, we cannot write an instance of a typeclass for a specialized version of a polymorphic type. The \[Char\] type is the polymorphic type \[a\] specialized to the type Char. We are thus prohibited from declaring \[Char\] to be an instance of a typeclass. This is highly inconvenient, since strings are ubiquitous in real code. 

The `TypeSynonymInstances` language extension removes this restriction, permitting us to write such instances. 

GHC supports another useful language extension, `OverlappingInstances`, which addresses the problem we saw with overlapping instances. When there are multiple overlapping instances to choose from, this extension causes the compiler to pick the most specific one. 

We frequently use this extension together with `TypeSynonymInstances`. Here's an example. 

```haskell
-- arquivo: src/Ch06.hs

class Foo a where
    foo :: a -> String

instance {-# OVERLAPS #-} Foo a => Foo [a] where
    foo = concat . intersperse ", " . map foo

instance Foo Char where
    foo c = [c]

instance Foo String where
    foo = id
```

(no 2010 mudou tambem a forma de fazer overlapping)

If we apply `foo` to a String, the compiler will use the String\-specific implementation. Even though we have an instance of Foo for \[a\] and Char, the instance for String is more specific, so GHC chooses it. For other types of list, we will see the behavior specified for \[a\]. 

With the `OverlappingInstances` extension enabled, GHC will still reject code if it finds more than one equally specific instance. 

![[Note]](/support/figs/note.png)

(reescrever toda essa parte)
When to use the OverlappingInstances extension

Here's an important point: GHC treats `OverlappingInstances` as affecting the declaration of an instance, _not_ a location where we use the instance. In other words, when we define an instance that we wish to allow to overlap with another instance, we must enable the extension for the module that contains the definition. When it compiles the module, GHC will record that instance as “can be overlapped with other instances”. 

Once we import this module and use the instance, we _won't_ need to enable `OverlappingInstances` in the importing module: GHC will already know that the instance was marked as “okay to overlap” when it was defined. 

This behaviour is useful when we are writing a library: we can choose to create overlappable instances, but users of our library do not need to enable any special language extensions. 

#### How does show work for strings?

The `OverlappingInstances` and `TypeSynonymInstances` language extensions are specific to GHC, and by definition were not present in Haskell 98. However, the familiar Show typeclass from Haskell 98 somehow renders a list of Char differently from a list of Int. It achieves this via a clever, but simple, trick. 

The Show class defines both a `show` method, which renders one value, and a `showList` method, which renders a list of values. The default implementation of `showList` renders a list using square brackets and commas. 

The instance of Show for \[a\] is implemented using `showList`. The instance of Show for Char provides a special implementation of `showList` that uses double quotes and escapes non-ASCII-printable characters. 

As a result, if someone applies `show` to a \[Char\] value, the implementation of `showList` will be chosen, and it will correctly render the string using quotes. 

At least sometimes, then, we can avoid the need for the `OverlappingInstances` extension with a little bit of lateral thinking. 

#### How to give a type a new identity


In addition to the familiar `data` keyword, Haskell provides us with another way to create a new type, using the `newtype` keyword. 

```haskell
-- arquivo: src/Ch06.hs
data DataInt = D Int
    deriving (Eq, Ord, Show)

newtype NewtypeInt = N Int
    deriving (Eq, Ord, Show)
```

The purpose of a `newtype` declaration is to rename an existing type, giving it a distinct identity. As we can see, it is similar in appearance to a type declared using the `data` keyword. 

![[Note]](/support/figs/note.png)

The type and newtype keywords

Although their names are similar, the `type` and `newtype` keywords have different purposes. The `type` keyword gives us another way of referring to a type, like a nickname for a friend. Both we and the compiler know that \[Char\] and String names refer to the same type. 

In contrast, the `newtype` keyword exists to _hide_ the nature of a type. Consider a UniqueID type. 

```haskell
-- arquivo: src/Ch06.hs
newtype UniqueID = UniqueID Int
    deriving (Eq)
```

The compiler treats UniqueID as a different type from Int. As a user of a UniqueID, we know only that we have a unique identifier; we cannot see that it is implemented as an Int. 

When we declare a `newtype`, we must choose which of the underlying type's typeclass instances we want to expose. Here, we've elected to make NewtypeInt provide Int's instances for Eq, Ord and Show. As a result, we can compare and print values of type NewtypeInt. 
```
ghci> N 1 < N 2
True
```
Since we are _not_ exposing Int's Num or Integral instances, values of type NewtypeInt are not numbers. For instance, we can't add them. 
```
 ghci> N 313 + N 37

<interactive>:1:0:
    No instance for (Num NewtypeInt)
      arising from a use of `+' at <interactive>:1:0-11
    Possible fix: add an instance declaration for (Num NewtypeInt)
    In the expression: N 313 + N 37
    In the definition of `it': it = N 313 + N 37
```
As with the `data` keyword, we can use a `newtype`'s value constructor to create a new value, or to pattern match on an existing value. 

If a `newtype` does not use automatic deriving to expose the underlying type's implementation of a typeclass, we are free to either write a new instance or leave the typeclass unimplemented. 

### Differences between data and newtype declarations

The `newtype` keyword exists to give an existing type a new identity, and it has more restrictions on its uses than the `data` keyword. Specifically, a `newtype` can only have one value constructor, and that constructor must have exactly one field. 

```haskell
-- arquivo: src/Ch06.hs
-- ok: any number of fields and constructors
data TwoFields = TwoFields Int Int

-- ok: exactly one field
newtype Okay = ExactlyOne Int

-- ok: type parameters are no problem
newtype Param a b = Param (Either a b)

-- ok: record syntax is fine
newtype Record = Record {
      getInt :: Int
    }

-- bad: no fields
--newtype TooFew = TooFew

-- bad: more than one field
--newtype TooManyFields = Fields Int Int

-- bad: more than one constructor
--newtype TooManyCtors = Bad Int | Worse Int
```

Beyond this, there's another important difference between `data` and `newtype`. A type created with the `data` keyword has a book-keeping cost at runtime, for example to track which constructor a value was created with. A `newtype` value, on the other hand, can only have one constructor, and so does not need this overhead. This makes it more space- and time-efficient at runtime. 

Because a `newtype`'s constructor is used only at compile time and does not even exist at runtime, pattern matching on `undefined` behaves differently for types defined using `newtype` than for those that use `data`. 

To understand the difference, let's first review what we might expect with a normal datatype. We are already familiar with the idea that if `undefined` is evaluated at runtime, it causes a crash. 

```
ghci> undefined
*** Exception: Prelude.undefined
```


Here is a pattern match where we construct a DataInt using the `D` constructor, and put `undefined` inside. 

```
ghci> case D undefined of D _ -> 1
1
```

Since our pattern matches against the constructor but doesn't inspect the payload, the `undefined` remains unevaluated and does not cause an exception to be thrown. 

In this example, we're not using the `D` constructor, so the unprotected `undefined` is evaluated when the pattern match occurs, and we throw an exception. 

```
ghci> case undefined of D _ -> 1
*** Exception: Prelude.undefined
```

When we use the `N` constructor for the NewtypeInt type, we see the same behaviour as with the DataInt type's `D` constructor: no exception. 

```
ghci> case N undefined of N _ -> 1
1
```

The crucial difference arises when we get rid of the `N` constructor from the expression, and match against an unprotected `undefined`. 

```
ghci> case undefined of N _ -> 1
1
```

We don't crash! Because there's no constructor present at runtime, matching against `N _` is in fact equivalent to matching against the plain wild card `_`: since the wild card always matches, the expression does not need to be evaluated. 

>Another perspective on newtype constructors
>
>Even though we use the value constructor for a `newtype` in the same way as that of a type defined using the `data` keyword, all it does is coerce a value between its “normal” type and its `newtype` type. 

>In other words, when we apply the `N` constructor in an expression, we coerce an expression from type Int to type NewtypeInt as far as we and the compiler are concerned, but absolutely nothing occurs at runtime. 

>Similarly, when we match on the `N` constructor in a pattern, we coerce an expression from type NewtypeInt to Int, but again there's no overhead involved at runtime. 

#### Summary: the three ways of naming types

Here's a brief recap of Haskell's three ways to introduce new names for types. 

*   The `data` keyword introduces a truly new albegraic data type. 
    
*   The `type` keyword gives us a synonym to use for an existing type. We can use the type and its synonym interchangeably. 
    
*   The `newtype` keyword gives an existing type a distinct identity. The original type and the new type are _not_ interchangeable. 
    

### JSON typeclasses without overlapping instances


Enabling GHC's support for overlapping instances is an effective and quick way to make our JSON code happy. In more complex cases, we will occasionally be faced with several equally good instances for some typeclass, in which case overlapping instances will not help us and we will need to put some `newtype` declarations into place. To see what's involved, let's rework our JSON typeclass instances to use `newtype`s instead of overlapping instances. 

Our first task, then, is to help the compiler to distinguish between \[a\], the representation we use for JSON arrays, and \[(String,\[a\])\], which we use for objects. These were the types that gave us problems before we learned about `OverlappingInstances`. We wrap up the list type so that the compiler will not see it as a list. 

\-- file: ch06/JSONClass.hs
newtype JAry a = JAry {
      fromJAry :: \[a\]
    } deriving (Eq, Ord, Show)



When we export this type from our module, we'll export the complete details of the type. Our module header will look like this: 

\-- file: ch06/JSONClassExport.hs
module JSONClass
    (
      JAry(..)
    ) where



The “`(..)`” following the JAry name means “export all details of this type”. 

![[Note]](/support/figs/note.png)

A slight deviation from normal use

Usually, when we export a `newtype`, we will _not_ export its data constructor, in order to keep the details of the type abstract. Instead, we would define a function to apply the constructor for us. 

\-- file: ch06/JSONClass.hs
jary :: \[a\] -> JAry a
jary = JAry



We would then export the type constructor, the deconstructor function, and our construction function, but not the data constructor. 

\-- file: ch06/JSONClassExport.hs
module JSONClass
    (
      JAry(fromJAry)
    , jary
    ) where



When we don't export a type's data constructor, clients of our library can only use the functions we provide to construct and deconstruct values of that type. This gives us, the library authors, the liberty to change our internal representation if we need to. 

If we export the data constructor, clients are likely to start depending on it, for instance by using it in patterns. If we later wish to change the innards of our type, we'll risk breaking any code that uses the constructor. 

In our circumstances here, we have nothing to gain by making the array wrapper abstract, so we may as well simply export the entire definition of the type. 

We provide another wrapper type that hides our representation of a JSON object. 

\-- file: ch06/JSONClass.hs
newtype JObj a = JObj {
      fromJObj :: \[(String, a)\]
    } deriving (Eq, Ord, Show)



With these types defined, we make small changes to the definition of our `JValue` type. 

\-- file: ch06/JSONClass.hs
data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)   -- was \[(String, JValue)\]
            | JArray (JAry JValue)    -- was \[JValue\]
              deriving (Eq, Ord, Show)



This change doesn't affect the instances of the JSON typeclass that we've already written, but we will want to write instances for our new JAry and JObj types. 

\-- file: ch06/JSONClass.hs
jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)

jaryToJValue :: (JSON a) => JAry a -> JValue

instance (JSON a) => JSON (JAry a) where
    toJValue = jaryToJValue
    fromJValue = jaryFromJValue



Let's take a slow walk through the individual steps of converting a JAry a to a JValue. Given a list where we know that everything inside is a JSON instance, converting it to a list of JValues is easy. 

\-- file: ch06/JSONClass.hs
listToJValues :: (JSON a) => \[a\] -> \[JValue\]
listToJValues = map toJValue



Taking this and wrapping it to become a JAry JValue is just a matter of applying the `newtype`'s type constructor. 

\-- file: ch06/JSONClass.hs
jvaluesToJAry :: \[JValue\] -> JAry JValue
jvaluesToJAry = JAry



(Remember, this has no performance cost. We're just telling the compiler to hide the fact that we're using a list.) To turn this into a JValue, we apply another type constructor. 

\-- file: ch06/JSONClass.hs
jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray



Assemble these pieces using function composition, and we get a concise one-liner for converting to a JValue. 

\-- file: ch06/JSONClass.hs
jaryToJValue = JArray . JAry . map toJValue . fromJAry



We have more work to do to convert _from_ a JValue to a JAry a, but we'll break it into reusable parts. The basic function is straightforward. 

\-- file: ch06/JSONClass.hs
jaryFromJValue (JArray (JAry a)) =
    whenRight JAry (mapEithers fromJValue a)
jaryFromJValue \_ = Left "not a JSON array"



The `whenRight` function inspects its argument: calls a function on it if it was created with the `Right` constructor, and leaves a `Left` value untouched. 

\-- file: ch06/JSONClass.hs
whenRight :: (b -> c) -> Either a b -> Either a c
whenRight \_ (Left err) = Left err
whenRight f (Right a) = Right (f a)



More complicated is `mapEithers`. It acts like the regular `map` function, but if it ever encounters a `Left` value, it returns that immediately, instead of continuing to accumulate a list of `Right` values. 

\-- file: ch06/JSONClass.hs
mapEithers :: (a -> Either b c) -> \[a\] -> Either b \[c\]
mapEithers f (x:xs) = case mapEithers f xs of
                        Left err -> Left err
                        Right ys -> case f x of
                                      Left err -> Left err
                                      Right y -> Right (y:ys)
mapEithers \_ \_ = Right \[\]



Because the elements of the list hidden in the JObj type have a little more structure, the code to convert to and from a JValue is a bit more complex. Fortunately, we can reuse the functions that we just defined. 

\-- file: ch06/JSONClass.hs
import Control.Arrow (second)

instance (JSON a) => JSON (JObj a) where
    toJValue = JObject . JObj . map (second toJValue) . fromJObj

    fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
      where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
    fromJValue \_ = Left "not a JSON object"





The dreaded monomorphism restriction
------------------------------------

The Haskell 98 standard has a subtle feature that can sometimes bite us in unexpected circumstances. Here's a simple function definition that illustrates the issue. 

\-- file: ch06/Monomorphism.hs
myShow = show



If we try to load this definition into **ghci**, it issues a peculiar complaint. 

    ghci> 



The “monomorphism restriction” to which the error message refers is a part of the Haskell 98 standard. _Monomorphism_ is simply the opposite of polymorphism: it indicates that an expression has exactly one type. The _restriction_ lies in the fact that Haskell sometimes forces a declaration to be less polymorphic than we would expect. 

We mention the monomorphism restriction here because although it isn't specifically related to typeclasses, they usually provide the circumstances in which it crops up. 

![[Tip]](/support/figs/tip.png)

Tip

It's possible that you will not run into the monomorphism restriction in real code for a long time. We don't think you need to try to remember the details of this section. It should suffice to make a mental note of its existence, until eventually GHC complains at you with something like the above error message. If that occurs, simply remember that you read about the error here, and come back for guidance. 

We won't attempt to explain the monomorphism restriction\[[14](#ftn.id610076)\]. The consensus within the Haskell community is that it doesn't arise often; it is tricky to explain; it provides almost no practical benefit; and so it mostly serves to trip people up. For an example of its trickiness, while the definition above falls afoul of it, the following two compile without problems. 

\-- file: ch06/Monomorphism.hs
myShow2 value = show value

myShow3 :: (Show a) => a -> String
myShow3 = show



As these alternative definitions suggest, if GHC complains about the monomorphism restriction, we have three easy ways to address the error. 

*   Make the function's arguments explicit, instead of leaving them implicit. 
    
*   Give the definition an explicit type signature, instead of making the compiler infer its type. 
    
*   Leave the code untouched, and compile the module with the `NoMonomorphismRestriction` language extension enabled. This disables the monomorphism restriction. 
    

Because the monomorphism restriction is unwanted and unloved, it will almost certainly be dropped from the next revision of the Haskell standard. This does not quite mean that compiling with `NoMonomorphismRestriction` is always the right thing to do: some Haskell compilers (including older versions of GHC) do not understand this extension, but they'll accept either of the other approaches to making the error disappear. If this degree of portability isn't a concern to you, then by all means enable the language extension. 

Conclusion
----------

_FIXME: needs extending to cover JSON_

In this chapter, you learned about the need for typeclasses and how to use them. We talked about defining our own typeclasses and then covered some of the important typeclasses that are defined in the Haskell library. Finally, we showed how to have the Haskell compiler automatically derive instances of certain typeclasses for your types. 

  

* * *

\[[12](#id603171)\] We provided a default implementation of both functions, which gives implementers of instances choice: they can pick which one they implement. We could have provided a default for only one function, which would have forced users to implement the other every time. As it is, users can implement one or both, as they see fit.

\[[13](#id604799)\] As you will see in [the section called “Lazy I/O”](io.html#io.lazy "Lazy I/O"), Haskell doesn't actually read the entire file at this point. But for the purposes of this example, we can ignore that distinction.

\[[14](#id610076)\] If you simply _must_ read the gory details, see [section 4.5.5](http://www.haskell.org/onlinereport/decls.html#sect4.5.5) of the Haskell 98 Report.
