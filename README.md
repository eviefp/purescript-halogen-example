## WIP -- remove once done.

Work in progress. Still to do:
- finish the overview in this file
- add lots of comments in these files, clean them up a bit as well

Should the API actually do a request somewhere on the internet? I think it's better to avoid that. I'd rather not
have this sample be too opinionated (for example by using Affjax or something).

# Purescript Halogen Example
Sample Halogen app with a few DSLs implemented as the application's free monad.

The overview presented here has some simplified examples. You will find some of the types
presented here have less arguments / constructors / etc. than the actual types used in the
source code.

## Purpose
This repository aims to be an example of how to use a `Free` monad with `Halogen` in order
to create a DSL that can be used inside the component's `eval`.

In this repository, you'll find examples for DLS's that allow you to:
- read some static environment (through `MonadAsk`)
- navigate to different routes, along with a routing component
- read or modify the global state of the application
- run queries on a (dummy) server API
- trigger the root component to show a dialog box and execute commands depending on action taken

## Our application's DSLs
Each DSL is expressed as a `typeclass`. These DSL's can define any number of functions, each of 
which should return a monadic result. For example, our `Navigation` DSL is defined as:

```purescript
class Monad m <= NavigationDSL m where
  navigate :: Route -> m Unit
```

The `navigate` function will most likely incur a side-effect, so we express that by returning `m Unit`.

In order to be able to use these DSL's in `Halogen`, we need to lift these operations to its monad, which is `HalogenM`:

```purescript
instance navigationDSLHalogenM :: NavigationDSL m => NavigationDSL (HalogenM s f g p o m) where
  navigate = HalogenM <<< liftF <<< Lift <<< navigate
```

What this basically says is that whenever you use `navigate` within a HalogenM context, we will
the DSL to the inner-monad `m`, which means we'll need to have an instance ourselves.

## Our application's Functor
Before we get our monad, we'll start by defining a Functor. This functor needs to encode whatever
is needed to execute the requested DSL. For example, `NavigationDSL` needs a `Route`, but we can't
create a functor like `data ExampleF = Navigate Route` because this is not a valid functor
instance.

Since our DSL does not return any value of interest, we need a _dummy_ value to return, so we 
will encode our functor as:

```purescript
data ExampleF a = Navigate Route a
```

## Free monad
This does not aim to be an exhaustive explanation of what a `Free` monad is. However, for the
purposes of this example:

Let's review `map` and `bindFlipped`:

```purescript
map         :: forall a b f. Functor f => (a ->   b) -> f a -> f b
bindFlipped :: forall a b m. Bind m    => (a -> m b) -> m a -> m b
```

If we were to use `map` instead of `bind` with functions that return monadic values, we'd
end up with:

```purescript
mapWithoutBind :: forall a b f. Functor f => (a -> f b) -> f a -> f (f b)
mapWithoutBind f a = f <$> a
```

However, consider we have the following type:

```purescript
data Free f a
  = Pure a
  | Bind (f (Free f a))
```

`Free` is a `Monad` for any functor `f`. It manages to do it by using its recursive data
type to create a pseudo-`bind`.

Please note that the actual implementation of `Free` is quite a bit different due to performance reasons. Please see [Reflection without Remorse](http://okmij.org/ftp/Haskell/zseq.pdf) for more details.

## Our own Free monad
We can define our own free monad as:

```purescript
newtype ExampleM a = ExampleM (Free ExampleF a)
```

We can now derive instances for `Functor`, `Apply`, `Applicative`, `Bind`, and `Monad` for free
(thanks to the `Free` monad).

We also need the `NavigationDSL` instance for `ExampleM`:

```purescript
instance navigationDSLAlerterM :: NavigationDSL (ExampleM eff env st) where
  navigate = ExampleM <<< liftF <<< flip Navigate unit
```

We basically store the `Route` and `unit` in our `Navigate Route a` constructor.

## Natural transform run

## Signaling back to main, etc.

## Brief overview of the other DSL's