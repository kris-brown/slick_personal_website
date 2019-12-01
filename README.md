# Slick Template

To use, `cd` into this directory and:
```shell
stack build
stack exec build-site
rsync -rv docs/ ksb@rice.stanford.edu:afs-home/WWW
```
A quick tool for serving your file system during development:

```shell
$ npm install -g serve
$ serve docs
```
