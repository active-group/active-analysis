FROM clojure:lein
COPY . /active-analytics
WORKDIR /active-analytics
RUN apt-get update \
    && apt-get install apt-transport-https \
    && wget https://apt.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB \
    && apt-key add GPG-PUB-KEY-INTEL-SW-PRODUCTS-2019.PUB \
    && sh -c 'echo deb https://apt.repos.intel.com/mkl all main > /etc/apt/sources.list.d/intel-mkl.list' \
    && apt-get update \
    && apt-get -y install intel-mkl-64bit-2018.2-046
ENV LD_LIBRARY_PATH /opt/intel/mkl/lib/intel64:/opt/intel/lib/intel64
RUN lein deps
CMD /bin/bash