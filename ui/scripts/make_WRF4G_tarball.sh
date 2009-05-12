
version="0.0.0"

mkdir tarball
cd tarball
  ln -s ../wn/* .
  ln -s ../share/* .
  tar czhv --exclude=".svn" -f ../WRF4G-${version}.tar.gz *
cd ..
rm -rf tarball
