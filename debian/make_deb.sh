#!/usr/bin/env bash
# SPDX-License-Identifier: MIT
# Copyright (c) 2020 Chua Hou

# Based on https://github.com/jgm/pandoc/blob/2.11.0.2/linux/make_artifacts.sh

set -e

# get into script directory
cd $(dirname $0)

# remove existing debs
rm *.deb || true

# get metadata
case "$(uname -m)" in
	x86_64) ARCHITECTURE=amd64 ;;
	i686)   ARCHITECTURE=i386  ;;
	i386)   ARCHITECTURE=i386  ;;
	*)      echo "Unknown architecture, failing" && false
esac
VERSION=$(stack query locals cpufreq-plugin version)

# folder to form file system for dpkg-deb
BUILD_DIR=build
rm -rf $BUILD_DIR
mkdir -p $BUILD_DIR/usr/bin
mkdir -p $BUILD_DIR/usr/share/doc/cpufreq-plugin
mkdir -p $BUILD_DIR/etc/sudoers.d

# build binaries
stack clean
stack build
stack install --local-bin-path $BUILD_DIR/usr/bin

# copy other files
cp copyright $BUILD_DIR/usr/share/doc/cpufreq-plugin/copyright
cp cpufreq $BUILD_DIR/etc/sudoers.d
sudo chown root:root $BUILD_DIR/etc/sudoers.d/cpufreq

# get installed size
INSTALLED_SIZE=$(du -k -s $BUILD_DIR/usr | awk '{ print $1 }')

# copy control file, replacing fields
mkdir -p $BUILD_DIR/DEBIAN
sed "s/VERSION/$VERSION/" control.in | \
	sed "s/ARCHITECTURE/$ARCHITECTURE/" | \
	sed "s/INSTALLED_SIZE/$INSTALLED_SIZE/" \
	> $BUILD_DIR/DEBIAN/control

# build deb
sudo dpkg-deb -Zgzip --build $BUILD_DIR
sudo chown $USER:$USER build.deb
dpkg-name build.deb

cd -
