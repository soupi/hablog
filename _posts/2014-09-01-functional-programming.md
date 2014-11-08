title: My Take on Functional Programming
author: Gil
tags: functional programming, code, haskell, scheme

Hello everybody, this is my first time posting this blog. I've tried to think about what to write about on this new blog and in the end decided to go with something I like to talk about - functional programming. I hope you'll enjoy this blog and this post.

I've been programming in [Haskell][1] on and off for the past year and recently I've started to look into [Scheme][4], 
and although I still consider myself as a Beginner/Intermediate functional programmer I would like to share my take on what Functional Programming is really all about.

![Functional Programming][funcimg]

## So what is Functional Programming?
Functional Programming is a programming paradigm, a style for writing computer programs. In this style, computation is viewed as the evaluation of expressions rather then a series of steps that changes the state of the computer. 
When trying to tackle a problem using the functional programming paradigm, instead of thinking about how to make the computer work and how should we change its current state in order to compute things, you start with some data and slowly manipulate it (or creata new data from it) until we get the answer we were looking for. in order to manipulate the data or expression, we have functions, which serve as the main means of composition and abstraction.

### Modularity and Higher-Order Functions
The thing I absolutely love about functional programming is the way of composing functions to get another function. since one can mix and match functions to create a bigger function it is very logical to make many very small functions that does only one thing and then compose them. the result is high modularity. For me, functional programming is about __combining__ little pieces of code (functions) that does something specific in order to make bigger pieces of code, until they are sufficient enough to express a solution to the problem I'm trying to solve.

Higher Order-Functions are functions that takes functions as input and returns a function as output. for example: map is a function that takes a function that manipulates an element (or returns a new element based on it), and returns a function that applies this function to every element in a container (a functor to be specific). this is highly useful and helps in increasing modularity and seperating responsibility since you seperate what you do to each element and how you apply that to each element in a container. This also encourages code reuse, it doesn't matter what you want to do to each element, you write the mapping function once, and use it for every function and any element you want, it doesn't change. It shouldn't change.


### Functional Languages
A Functional Programming Language is a programming language that encourages using functional style when writing programs. In these languages, writing in functional style is rewarded by being easier to write, being more concise and being better optimized. for example, higher order functions are possible and easy to write, tail call optimization is guaranteed, every computation is an expression and more.

The benefits of knowing how to write well in functional style is significant even for non-functional programmers. since functional style promotes building very small components and combine them to larger components, a programmer well-versed in functional style is able to identify and seperate responsibilities easily, it is the bread and butter of functional programming. Though, there are some set backs. For example, when I run into a programming language that let's me combine functions easily (like Haskell's (.) function) I feel free an able to plan how to structure my code more easily. But when I run into a language that doesn't let me combine functions so easily (for example PHP or Java), I feel like I'm struggling to express my ideas and that I need to think very carefully how to structure my code.


Functional progamming is about looking at programming from a different prespective. It is not about how I tell the computer what to do in order to get a result, it is about describing what things are with expressions rather than statements that changes a state. Functional programming lets me describe my ideas in a way that is closer to how I think rather than how the computer thinks. It feel easier for me to write this kind of code, it is easier to reason about and it is easier to change. 

### Conclusion

In this post, I didn't touch many topics regarding functional programming, I didn't talk about the use of recursion instead of loops, about purely functional style - in which every function can only take input and produce output and nothing else, which means it cannot affect anything in your program and how this is super useful and a great habit, I didn't talk about referential transparency nor on Currying, I didn't even list all the things I didn't talk about. There is simply a lot to learn!

So I'd recommend any programmer to learn functional programming. Especially now, since you may find that most recent features mainstream languages adopts today, such as: tail recursion, pattern matching, immutibility and lambda expressions, already existed long ago in many functional languages. So if you want to be one (or more) steps ahead of everyone else, learn a functional language.

I started learning about Functional Programming with [Learn You A Haskell For Great Good][3] and I definitely recommend it to anyone interested in functional programming and Haskell. Another book I think any programmer should read at some point (better early than late) is [SICP][2]. It's a wonderful classic book about software developement and computer science and since it is taught in Scheme, you'll learn functional programming as well. Beyond that, the internet is full of functional programming articles, books, blog posts and so on. If you got to this article, I'm sure you can get to other sources as well.




[1]: http://haskell.org
[2]: http://mitpress.mit.edu/sicp/
[3]: http://learnyouahaskell.com/
[4]: http://www.schemers.org/
[funcimg]: images/funcprog.png
