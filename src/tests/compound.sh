#! /bin/sh
wc -l "$srcdir"/*.setl | tail -1 | awk '{print $1}'
