# 01
##### Lambda Calculus
##### Functional Programming
##### Functions
##### Lambda Terms
##### Beta Reduction

<hr><hr>
## The Lambda Calculus
##### Formalizes the concept of effective computability, thus determining which problems, or classes of problems, can be solved
</br>

- A **calculus** is a method of calculation or reasoning.
- The **lambda calculus** is one process for formalizing a method.

</br><hr><hr>
## Functional Programming
##### A computer programming paradigm that relies on functions modeled on mathematical functions
</br>

- Functional programming languages are all based on the lambda calculus.
- Programs are a combination of **expressions**.

###### Purity / Referential Transparency
- Referential transparency means that the same function, given the same values to evaluate, will always return the same result in pure functional programming, as they do in math. 
- Haskell is a _pure_ functional language because it does not incorporate features that arenβt translatable into lambda expressions.

###### Expressions
- Expressions include concrete values, variables, and also functions.

###### Functions
- Function are expressions that are applied to an argument or input, and once applied, can be _reduced_ or _evaluated_.
-- Functions are _first-class_ be used as values or passed as arguments, or inputs, to yet more functions.


</br><hr><hr>
## Functions
##### Defines and represents the relationship between a set of possible inputs and a set of possible outputs
</br>

- Applying a function such as addition to two inputs maps those two inputs to an output β the sum of those numbers.

</br>

The function `π` defines the following relations: 

- The first value is the input.
- The second value is the output.

```
π(1) = π΄ 
π(2) = π΅ 
π(3) = πΆ
```

- The input set (domain) is `{1, 2, 3}` 
- The output set (codomain) is `{π΄, π΅, πΆ}`.
- `π` will always return the value `π΄` given the input `1` - no exceptions.


</br><hr><hr>
## Lambda Terms
##### Expressions, variables, and abstractions are the three basic components of the lamba calculus
</br>


###### Expressions
- The word _expression_ refers to a superset of all three components.
-- Can be a variable name, an abstraction, or a combination of those things.

###### Variables
- Variables here have no meaning or value.
- They are just names for potential inputs to functions.
- The simplest expression is a single variable.

###### Abstractions
- An _abstraction_ is a function. 
-- It is an _anonymous function_ which has no name.
- Abstractions consist of two parts, a head and a body.
-- The head of the function is a π (lambda) followed by a variable name.
-- The body of the function is another expression.
- Abstractions are applied to an argument.
--- An _argument_ is an input value.

```
ππ₯.π₯
```

- The variable named in the head is the _parameter_ and _binds_ all instances of that same variable in the body of the function.
- Applying this function to an argument each `π₯` in the body of the function will have the value of that argument.

```
Ξ» x . x
^___^       
            the extent of the head of the lambda.

Ξ» x . x
  ^_______    
            the single parameter of the function. This
            binds any variables with the same name
            in the body of the function.

Ξ» x . x
      ^___    
            body, the expression the lambda returns
            when applied. This is a bound variable.
```

The dot (`.`) separates the parameters of the lambda from the function body.

- The abstraction as a whole has no name.
- The reason we call it an abstraction is that it is a generalization (or abstraction) from a concrete instance of a problem, and it abstracts through the introduction of names.
- The names stand for concrete values. 
-- By using named variables we allow for the possibility of applying the general function to different values.
- When we apply the abstraction to arguments, we replace the names with values, making it concrete.

##### Alpha Equivalence

- The variable `π₯` here is not semantically meaningful except in its role in that single expression.

```
ππ₯.π₯ 
```

- A form of equivalence between lambda terms called alpha equivalence.
-- Meaning all of these functions are the same thing:

```
ππ₯.π₯ 
ππ.π 
ππ§.π§
```

</br><hr><hr>
## Beta Reduction
##### Substituting the input expression for all bound variables within the body and eliminating the head of the abstraction
</br>


</br><hr><hr>
## Free Variables
##### Sometimes the body expression has variables that are not named in the head
</br>


```
ππ₯.π₯π¦ 
```

- When we apply this function to an argument, nothing can be done with the π¦. It remains irreducible.

That whole abstraction can be applied to an argument, `π§`, like this: `(ππ₯.π₯π¦)π§`.

1. `(ππ₯.π₯π¦)π§`
  -- We apply the lambda to the argument π§.
2. `(π[π₯ βΆ= π§].π₯π¦)`
  -- Since `π₯` is the bound variable, all instances of `π₯` in the body of the function will be replaced with `π§`. The head will be eliminated, and we replace any `π₯` in the body with a π§.
3. `π§π¦`
  -- The head has been applied away, and there are no more heads or bound variables. Since we know nothing about `π§` or `π¦`, we can reduce this no further.

</br><hr><hr>
## Multiple Arguments
##### Currying with Nested Lambdas
</br>

```
ππ₯π¦.π₯π¦
```
is shorthand for

```
ππ₯.(ππ¦.π₯π¦)
```

```
1. (ππ₯π¦π§.π₯π§(π¦π§))(πππ.π)(ππ.π)
2. (ππ₯.ππ¦.ππ§.π₯π§(π¦π§))(ππ.ππ.π)(ππ.π)
3. (ππ¦.ππ§(ππ.ππ.π)π§(π¦π§))(ππ.π)
4. ππ§(ππ.ππ.π)(π§)((ππ.π)π§)
5. ππ§(ππ.π§)((ππ.π)π§)
6. ππ§.π§

1. x => y => z => x(z)(y(z))(m => n => m)(p)

2. (x => y => z => x(z)(y(z))(m => n => m)(p => p)

3. x = (m => n => m)
   (y => z => (m => n => m)(z)(y(z))(p => p)

4. y = (p => p)
   z => (m => n => m)(z)((p => p)z)

5. m = z
   z => (n => z)((p => p)z)

6. n = ((p => p)z)
   z => z

```

</br><hr><hr>
## Combinators
##### A lambda term with no free variables.
</br>

</br><hr><hr>
## Divergence
##### The reduction process never terminates or ends.
</br>




