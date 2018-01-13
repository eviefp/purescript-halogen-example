Work in progress. Still to do:
- ShowDialog API as a way to pass our monad as "actions"
- lots of comments in these files, clean them up a bit as well
- some sort of overview in this file

Should the API actually do a request somewhere on the internet? I think it's better to avoid that. I'd rather not
have this sample be too opinionated (for example by using Affjax or something).

Sample Halogen app with a few DSLs implemented as the application's free monad.