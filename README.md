## WARNING
This repository is currently being updated for 0.12. The code is (mostly) done, but the documentation (and possibly comments) are out of date.

# Purescript Halogen Example
Sample Halogen app with a few DSLs implemented as the application's reader monad.

The overview presented here has some simplified examples. You will find some of the types
presented here have less arguments / constructors / etc. than the actual types used in the
source code.

You can check out the result [here](https://vladciobanu.github.io/purescript-halogen-example/index.html).

## Purpose
This repository aims to be an example of how to use an application monad with `Halogen` in order
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
  navigate = lift <<< navigate
```

What this basically says is that whenever you use `navigate` within a HalogenM context, we will
lift the DSL to the inner-monad `m`, which means we'll need to have an instance ourselves.

## The Reader monad

TODO: short motivation of why we chose ReaderT and Environment.

## Our application's monad
We can define our monad as:

```purescript
newtype ExampleM a = ExampleM (ReaderT Environment Aff a)
```

We can now derive instances for `Functor`, `Apply`, `Applicative`, `Bind`, `Monad`, `MonadEffect`,
and `MonadAff` for free.

We also need the `NavigationDSL` instance for `ExampleM`:

```purescript
instance navigationDSLExampleM :: NavigationDSL ExampleM where
  navigate route = ExampleM do
    env <- ask
    liftEffect $ env.push $ PushRoute route
```

We basically use our environment's `push` to send the new route to the event listener.

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
follows we will write:

```purescript
runExampleM :: forall a. ExampleM a -> Environment -> Aff a
runExampleM m env = runReaderT (unwrap m) env
```

All that remains is somehow figure out how to do the route change through an `Eff` or `Aff`.
Which brings us to...

## Signaling back to main
Some of our DSLs might need to signal back to main. The basic idea is we create a new event in
`main` and we pass the function that can `push` to this event in the `environment` to our
`runExampleM` transform. This means we'll have a way of sending messages to our `main`.

Back in `main`, we'll have to handle them somehow. And since we also create the root component
there, we could use its driver's query to send actions to that component.

Back in main:

```purescript
main = HA.runHalogenAff do
  body  <- HA.awaitBody
  state <- liftEffect $ Ref.new 0
  event <- liftEffect create
  let environment =
        { token: APIToken secretKey
        , answer: 42
        , state
        , push: event.push
        }

  let router' = H.hoist (flip runExampleM environment) R.component
  driver <- runUI router' unit body
  liftEffect $ subscribe event.event (handler driver)
```

We omitted the definition for `handler` for brevity. You can check the `Main.purs` file for details.
The main idea is we create the `environment`, which has everything `ExampleM` needs to run everything
we care about.

The `handler` function sends messages to the `router` (the main Halogen component), depending on its
input (one message is for changing the current route, and the other is for showing a dialog box).

## MonadAsk
`MonadAsk` allows us to get the current environment. This is initialized in `main` and
passed down to our `runExampleM` function.

## StateDSL
Unfortunately, we can't use `MonadState` because `HalogenM` already has an instance for it
for each component's state. We need to define our own state monad, and one way is presented
in the `StateDSL` class.

## ServerAPI
We have a dummy `API` method in the `Example.Server.ServerAPI` module, but it could
be a function that does an `Ajax` request just as well. We assume our API needs a
token, but our DSL does not ask for it.

## ShowDialog
This is an example where we pass `actions` as our `ExampleM` monad. Basically, we
want our root component to show a dialog with a custom set of buttons. Each of
these buttons has an action that will run under `ExampleM`, which means it can run
all the DSLs that we define.

The way this works is we initially send these actions as the dialog  options,
under any monad `m`. The `ExampleM` instance for `DialogDSL` assumes
both the monad that it runs under and the monad used to run the actions
is `ExampleM`. It transforms the options to `Aff` and pushes them to the `router`
through the handler stored in the `environment`.
