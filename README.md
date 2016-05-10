pure
====

> pure -- zero-configuration **pu**sh **re**lay for github repositories

Pure forwards pushes to a Github repository on to a remote target. The
target could be a Github fork, a clone hosted on another service to be
kept in sync with Github, or it could be used to kick off a standard Git
[post-receive](https://git-scm.com/book/en/v2/Customizing-Git-Git-Hooks)
hook. In this way *pure* can be used for a kind of primitive CI system,
triggering scripts in response to events using existing mechanisms 
built-in to Git, which are otherwise unavailable to repositories hosted
on Github.

Simplicity is the highest virtue. Pure doesn't store any configuration
per-repository, the [webhook](https://developer.github.com/webhooks/)
URL is used to specify the target and a **key identifier**. These URLs
may be used on an ad-hoc basis, no other configuration is required. 


## Security

To avoid the risk exposing access credentials in the webhook URL, pure
uses a *disposable key* approach to securing requests. Requesting `/key`
from the pure server returns a JSON object as follows:

```
  { "name":    "bE15q1Z9c1b6OuC5Y6KGdEba"
  , "secret":  "Y!%IqKHv}g3200TP?U`0pO7D)t-*'z;H*b&d4gag}KIi[...]"
  , "pubKey":  "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDcbdR5[...]"
  }
```

The `name` is used to reference the key in a webhook URL. The `secret`
text must be supplied along with the webhook URL to Github. This ensures
that only genuine calls initiated by Github to pure are acted upon. The
`pubKey` field is an SSH public key. This key must be given access to
both source and destination repositores, as pure uses SSH to clone and
push repositories.

Keys are cheap and not intended to last forever. It is recommended you
decide on an expiry period and set up a cronjob to periodically remove
old keys.

## Compiling from source

*pure* uses [stack](http://docs.haskellstack.org/en/stable/README/) to
simplify the build process. Once you have stack installed, simply run:

`stack build`

If you are running stack for the first time, you may need to install a
compatible Haskell toolchain first. Fortunately, stack makes this easy:

`stack setup`

Be sure to run this from inside the checked-out repository to ensure you
get the right version.


