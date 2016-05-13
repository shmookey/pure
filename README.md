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


## Quickstart

1. Get *pure* up and running. For now this means compiling from source
   and running it manually.
2. Request a key from the keystore API: `curl myserver.com:3000/keys`.
   Keep this information safe somewhere.
3. Add the generated SSH public key to the list of authorized keys on
   the destination repository host.
4. Add the webhook to your Github repository under **Settings** / 
   **Webhooks & Services**. The **Payload URL** will be of the form:
   `http://myserver.com:3000/push/user@host/path/to/repo?key=<key_id>`
   where `<key_id>` refers to the `name` of the key we just generated.
   The **Secret** refers to the `secret` of our new key. You can leave
   everything else as defaults.

That's it! You're ready to go. Github will ping the webhook URL, and if
it's accessible, future pushes to that repository will be pushed along
to the destination you specified in the URL.

## Usage

```
pure - push relay for Github repositories

Usage: pure [-c|--config-file FILE] [-d|--daemon]
  Automatically push Github commits to a remote repository.

Available options:
  -h,--help                Show this help text
  -c,--config-file FILE    Path to pure.conf
  -d,--daemon              Run as a background process (daemon mode)
```

If run as a service, *pure* supports socket activation via systemd for
faster boot times and run-on-demand use cases. Sample unit files for
systemd are provided in `/service`.

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

## Configuration

See the provided `pure.conf` for an example of how the configuration
file is formatted. The following options are available:

- `server.port` - The port to listen on. Default: `3000`
- `server.logFile` - Where to write logs. Default: `/var/log/pure.log`
- `server.logLevel` - Events with lower priorities than this will not
  be logged. Possible values: `error`, `warn`, `info`, `debug`, `trace`.
  Default: `info`
- `server.pidFile` - Location of PID file to use when running in daemon
  mode. Default: `/var/run/pure/pure.pid`.
- `repocache.path` - Directory in which to store cloned repositories.
  Default: `/var/run/pure/repos`
- `keystore.path` - Directory in which to store secret keys. Default:
  `/var/run/pure/keys`

## Compiling from source

*pure* uses [stack](http://docs.haskellstack.org/en/stable/README/) to
simplify the build process. Once you have stack installed, simply run:

`stack build`

If you are running stack for the first time, you may need to install a
compatible Haskell toolchain first. Fortunately, stack makes this easy:

`stack setup`

Be sure to run this from inside the checked-out repository to ensure you
get the right version. Once you're all set up and successfully compiled,
you can launch *pure* via stack:

`stack exec pure -- -c pure.conf`

Note that when you run *pure* this way its command-line arguments are
separated from arguments to `stack exec` with a double dash.

