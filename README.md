# Req Conduit

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)
[![Hackage](https://img.shields.io/hackage/v/req-conduit.svg?style=flat)](https://hackage.haskell.org/package/req-conduit)
[![Stackage Nightly](http://stackage.org/package/req-conduit/badge/nightly)](http://stackage.org/nightly/package/req-conduit)
[![Stackage LTS](http://stackage.org/package/req-conduit/badge/lts)](http://stackage.org/lts/package/req-conduit)
[![Build Status](https://travis-ci.org/mrkkrp/req-conduit.svg?branch=master)](https://travis-ci.org/mrkkrp/req-conduit)
[![Coverage Status](https://coveralls.io/repos/mrkkrp/req-conduit/badge.svg?branch=master&service=github)](https://coveralls.io/github/mrkkrp/req-conduit?branch=master)

This library extends functionality of
the [`req`](https://hackage.haskell.org/package/req) package
with [`conduit`](https://hackage.haskell.org/package/conduit) helpers for
streaming big request bodies in constant space.

## Potential issues

Streaming of request body does not happen in constant memory. But it does
not work with `http-conduit` either, see:
https://github.com/snoyberg/http-client/issues/240. Streaming of response
body does happen in constant memory as expected. See the benchmarks coming
with the library for hands-on experiences.

## Contribution

Issues, bugs, and questions may be reported in [the GitHub issue tracker for
this project](https://github.com/mrkkrp/req-conduit/issues).

Pull requests are also welcome and will be reviewed quickly.

## License

Copyright © 2016 Mark Karpov, Micheal Snoyman

Distributed under BSD 3 clause license.
