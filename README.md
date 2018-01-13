## WIP -- remove once done.

Work in progress. Still to do:
- add some comments the source files, clean them up a bit as well

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
Each DSL is expressed as a `typeclass`. These DSLs can define any number of functions, each of 
which should return a monadic result. For example, our `Navigation` DSL is defined as:

```purescript
class Monad m <= NavigationDSL m where
  navigate :: Route -> m Unit
```

The `navigate` function will most likely incur a side-effect, so we express that by returning `m Unit`.

In order to be able to use these DSLs in `Halogen`, we need to lift these operations to its monad, which is `HalogenM`:

```purescript
instance navigationDSLHalogenM :: NavigationDSL m => NavigationDSL (HalogenM s f g p o m) where
  navigate = HalogenM <<< liftF <<< Lift <<< navigate
```

What this basically says is that whenever you use `navigate` within a HalogenM context, we will
lift the DSL to the inner-monad `m`, which means we'll need to have an instance ourselves.

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
Halogen needs to know how to effectively run our monad, which is expressed by its `hoist`
function:

```purescript
hoist
  :: forall h f i o m m'
   . Bifunctor h
  => Functor m'
  => (m ~> m')
  -> Component h f i o m
  -> Component h f i o m'
```

What this does, basically, is given a component that runs under monad `m` and a way to go
from `m` to `m'` (through the natural transform `m ~> m'`), then we can construct the
component that runs under monad `m'`.

It's also worth noting that `runUI` assumes a component that runs under the `Aff` monad,
so that means `m'` needs to be `Aff`. And since `m` is our own monad, `ExampleM`, it
follows we will write the transform:

```purescript
runExampleM :: forall eff. ExampleM ~> Aff (HalogenEffects eff)
runExampleM (ExampleM f) = foldFree <<< go $ f

  where

  go :: ExampleF ~> Aff (HalogenEffects eff)
  go (Navigate route a) = do
    -- actually do the route change somehow
    pure a
```

All that remains is somehow figure out how to do the route change through an `Eff` or `Aff`.
Which brings us to...
## Signaling back to main
Some of our DSLs might need to signal back to main. As with `Free`, this guide is not meant as 
a tutorial on `purescript-behaviors`. However, the basic idea is we create a new event in
`main` and we pass the function that can `push` to this event to our `runExampleM` transform.
This means we'll have a way of sending messages to our `main`.

Back in `main`, we'll have to handle them somehow. And since we also create the root component
there, we could use its driver's query to send actions to that component.

Since we are using a router as our main component, this all fits in great. So all we need to do
is create an event that accepts a `Route` in main and then alter our `runExampleM` function
to take it as a parameter. Then, inside the inner `go` natural transform we can replace the
comment with `liftEff $ push route`.

Back in main:

```purescript
main = HA.runHalogenAff do
  body  <- HA.awaitBody
  event <- liftEff create
  let router' = H.hoist (runExample event.push) Router.component
  driver <- runUI router' unit body
  liftEff $ subscribe event.event (handler driver)

  where 
  handler driver route = do
    _ <- launchAff $ driver.query <<< H.action <<< Goto $ route
    pure unit
```

## Brief overview of the other DSLs
There's a couple of other DSLs defined in this example.

### MonadAsk
`MonadAsk` allows us to get the current environment. This is initialized in `main` and
passed down to our `runExampleM` function.

Inside `ExampleF`, we encode it as the `Ask (env -> a)` constructor, and run it through
the `id` identity function (so then `a` basically also becomes `env` and we return the
environment passed down from `main`).

### StateDSL
Unfortunately, we can't use `MonadState` because `HalogenM` already has an instance for it
for each component's state. We need to define our own state monad, and one way is presented
in the `StateDSL` class.

### ServerAPI
We have a dummy `API` method in the `Example.Server.ServerAPI` module, but it could
be a function that does an `Ajax` request just as well. We assume our API needs a
token, but our DSL does not ask for it.

We connect the DSL to the actual implementation in our `ExampleM` instance, and we
pass the token in the `runExampleM` transform.

### ShowDialog
This is an example where we pass `actions` as our `ExampleM` monad. Basically, we
want our root component to show a dialog with a custom set of buttons. Each of
these buttons has an action that will run under `ExampleM`, which means it can run
all the DSLs that we define.

The way this works is we initially just store these actions within the dialog 
options, under any monad `m`. The `ExampleM` instance for `DialogDSL` assumes
both the monad that it runs under and the monad used to run the actions
is `ExampleM` and stores these options in a constructor.

The `runExampleM` transform uses the same `push` event that was used for navigation,
but in addition it has to transform the actions from `ExampleM` to `Aff`, and it
does so by running the transform again on each action.