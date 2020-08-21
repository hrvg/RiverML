[![Build Status](https://travis-ci.com/hrvg/RiverML.svg?token=Dx1gYTrTiuxgW9Sq3s3q&branch=master)](https://travis-ci.com/hrvg/RiverML)

# `RiverML`

## Purpose

This repository documents machine learning pipelines developped for fluvial geomorphology in the context of [California's environmental flows project](https://eflows.ucdavis.edu/).

## How to install

```
# Install development version from GitHub
devtools::install_github("hrvg/RiverML")
```

It is also likely you'll need to install the latest `terra` or `raster` version which has dependencies on GDAL (>= 3.0.0), GEOS (>= 3.3.0) and Proj.4 (>= 6.0.0).

On Linux, you can do:

```
sudo add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable
sudo apt-get -q update
sudo apt-get -y install libgdal-dev libgeos-dev libproj-dev 
```

The `terra` [github page](https://github.com/rspatial/terra) has some insights for other operating systems.

## How to use

Check out the [package's vignette to get started](articles/RiverML.html).