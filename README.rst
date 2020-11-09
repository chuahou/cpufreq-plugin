##############
cpufreq-plugin
##############

|License: MIT|

*cpufrequtils query and adjustment plugin*

A small program used for easily querying and adjusting CPU max frequency and
governor using cpufrequtils, intended to be used as a plugin for bars such as
polybar or i3blocks.

Warnings
========

The debian package is **DANGEROUS**. It will install a file in
/etc/sudoers.d, which can potentially break your sudo system!

Usage
======

Get current policy::

	cpufreq-plugin

Change governor::

	cpufreq-plugin gov

Increase / decrease max frequency::

	cpufreq-plugin increase
	cpufreq-plugin decrease
	cpufreq-plugin increase 500 # units in MHz
	cpufreq-plugin decrease 500

Dependencies
============

* cpufrequtils (``apt-get install cpufrequtils``)
* cabal
* ghc-8.8.4

.. |License: MIT| image:: https://img.shields.io/badge/License-MIT-yellow.svg
	:target: https://opensource.org/licenses/MIT
