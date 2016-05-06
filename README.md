pure
====

Pure is a *pu*sh *re*lay for Github repositories. It provides a *push*
[webhook](https://developer.github.com/webhooks/) which simply pushes
to another repository whenever the Github repository is pushed to. The
target could be a fork or a repository hosted on another service to be
kept in sync with Github, or it could be used to kick off a standard Git
[post-receive](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
hook. In this way *pure* can be used for a kind of primitive CI system
using Git's existing mechanisms for triggering scripts in response to
events, which are otherwise not available for repositories hosted on
Github.

## Compiling from source

*pure* uses [stack](http://docs.haskellstack.org/en/stable/README/) to
simplify the build process. Once you have stack installed, simply run:

`stack build`

If you are running stack for the first time, you may need to install a
compatible Haskell toolchain first. Fortunately, stack makes this easy:

`stack setup`

Be sure to run this from inside the checked-out repository to ensure you
get the right version.


