# bonbon

Examine some probablities in a "The Bonboniere" fun card game. The card game model is powered by `clojure.spec`.

## Prerequisites

You will need [Leiningen][1] 2.0 or above installed.

[1]: https://github.com/technomancy/leiningen

## Usage

To output stats:

    lein run

To output stats for a specific number of samples:

    lein run 100000

To run stats from the REPL:

    lein repl
    (bonbon.core/stats 10000)

To show a random hand along with it stats:

    (bonbon.core/stats 1 true)
    ;===============================================
    ;
    ;| :bonbon.core/praline | :bonbon.core/flavour |
    ;|----------------------+----------------------|
    ;|             :desiree |       #{:red :brown} |
    ;|             :desiree |           #{:yellow} |
    ;|             :desiree |            #{:green} |
    ;|              :lingot |       #{:red :brown} |
    ;|               :manon |           #{:yellow} |
    ;|             :mystere |              #{:red} |
    ;|             :mystere |       #{:red :brown} |
    ;=> {:same-kind-unique-flavours-1 100.0,
    ;    :unique-kind-same-flavour-1 100.0}

## License

Copyright © 2018 Kamil Toman

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
