# Active Analytics

A Clojure library containing analytic methods used in our projects. 

## Usage

So far, there are two implemented clustering algorithms:

- Power iteration clustering [(PIC)](http://www.cs.cmu.edu/~frank/papers/icml2010-pic-final.pdf): `active-analytics.clustering.pic`
- [k-means clustering](https://en.wikipedia.org/wiki/K-means_clustering): `active-analytics.clustering.k-means`

At the moment, the PIC implementation as well as the default k-means
centroid function only support [neanderthal](https://neanderthal.uncomplicate.org/)
as backend. Please follow its [instructions](https://neanderthal.uncomplicate.org/articles/getting_started.html#the-native-library-used-by-neanderthals-native-engine) 
on how to install the needed native Intel MKL library.

TODO

## License

Copyright © 2018 Active Group GmbH

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
