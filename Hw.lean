import USCMath587.String.Basic

/-
A structure is a special case of a inductive type that has only one constructor, here
`Alphabet.mk`. The `Alphabet` structure has two fields, `char` which is a Lean character
and `isUpperCase` which can be viewed as a proof that the character is upper case.

Given `(a : Alphabet)`, we can access the fields using "dot-notation"
`a.char` and `a.isUpperCase` or using `Alphabet.char a` and `Alphabet.isUpperCase a`.
-/
structure Alphabet where
  char : Char
  isUpperCase : char.isUpper

/-
This is very analogous to `Char` itself which is defined in the core library as
```lean
/-- The `Char` Type represents an unicode scalar value.
    See http://www.unicode.org/glossary/#unicode_scalar_value). -/
structure Char where
  /-- The underlying unicode scalar value as a `UInt32`. -/
  val   : UInt32
  /-- The value must be a legal codepoint. -/
  valid : val.isValidChar
```
The underlying value is a number so arithmetic operations become natural. We
just need to account for not overflowing the alphabet range.
-/

/--
This converts a string to a list of `Alphabet` values. The `fold` function is one
of the trinity of list operations in functional programming, the other two being `map`
and `filter`. The imperative version of `foldr` is a loop appends elements to an accumulator.
Lean is a functional language.
-/
def String.toAlphabetList (s : String) : List Alphabet :=
  -- We start with the empty list and then specify a function that takes
  -- what we have so far and the next character in the string and returns
  -- the next list.
  s.foldr (init := []) fun c acc =>
    -- Branching can be done with `if`, `else if`, and `else`. We name the
    -- hypothesis that `c` is an upper case letter `h` so we can use it later.
    if h : c.isUpper then
    -- The bare braces `{ ... }` are syntactic sugar for `Alphabet.mk ...`.
    -- The `::` is syntactic sugar for `List.cons`.
      { char := c, isUpperCase := h } :: acc
    else if h : c.isLower then
    -- Here we use the `toUpper` function to convert the lower case letter to upper case.
    -- And `toUpper_isUpper_of_isLower` is a proof that the result is upper case.
      { char := c.toUpper, isUpperCase := c.toUpper_isUpper_of_isLower h } :: acc
    -- We drop any characters that are not letters.
    else acc

def AlphabetList.toString (l : List Alphabet) : String :=
  l.foldl (init := "") fun acc a => acc.push a.char

/-
Under the hood an `instance` is just a `def` or a `theorem`. Labelling it as an instance
helps Lean automatically use it when needed.

The `ToString` instance also allows Lean to `#eval` expressions of the type `Alphabet`
or `List Alphabet`.
-/
instance : ToString Alphabet where
  toString a := a.char.toString

/-
We want to print our `List Alphabet` in groups of 5, similar to the textbook.
-/
instance : ToString (List Alphabet) where
  toString l :=
    let raw := l.foldl (init := "") fun acc a => acc ++ toString a
    String.intercalate " " <| raw.chunks 5

/-
Your assignment for this week is to implement the `shift` function below. It should
take an `Alphabet` and `n` and return a new `Alphabet` that is `n`
positions to the right in the alphabet wrapping around.
For example, `shift 'A' 1` should return `'B'`.

The modulo operator `%` may be your friend here. If you forget what codepoints
correspond to which characters, try e.g. `#eval 'A'.toNat`.
-/
def Alphabet.shift (a : Alphabet) (n : Nat) : Alphabet :=
  -- We can declare local variables with `let`
  let shifted : Nat := sorry -- fill this in with the correct natural number
  -- We can use `have` but the Lean only remembers the type and not the actual term
  -- `have`'s are useful for proofs
  have valid : shifted.isValidChar := sorry -- `by omega` will work with the correct `shifted`
  -- `omega` is a tactic that can solve linear arithmetic problemes for natural numbers and
  -- integers
  { char := Char.ofNatAux shifted valid,
    isUpperCase := by -- `by` enters tactic mode to provide a proof
      simp [Char.isUpper] -- `simp` is the simplifier, it will unfold definitions and apply lemmas
      -- either from its database or provided by the user in the call
      change 65 ≤ shifted ∧ shifted ≤ 90 -- `change` attempts to change the goal to something that
      -- is definitionally equal to the current goal
      sorry } -- finish this proof with `omega` when you have the correct `shifted`

/-
Finally we encode our string by converting it to a list of `Alphabet` values, shifting
each one, and then converting the list back to a string.
-/
def encode (s : String) (n : Nat) : String :=
  toString (s.toAlphabetList.map fun a => a.shift n)

