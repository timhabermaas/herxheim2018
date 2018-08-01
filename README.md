# Herxheim2018

[![Build Status](https://travis-ci.org/timhabermaas/herxheim2018.svg?branch=master)](https://travis-ci.org/timhabermaas/herxheim2018)

## Installation

```sh
$ stack setup
$ stack build
```

## Run the server

```sh
$ createdb herxheim2018
$ env PORT=8080 ADMIN_PASSWORD=admin DATABASE_URL='postgres://localhost/herxheim2018' stack exec herxheim2018-exe
```

## Run the tests

```sh
$ createdb herxheim2018_test
$ env DATABASE_URL='postgres://localhost/herxheim2018_test' stack test
```

## Deployment

The application is currently deployed to Heroku. For deployment run:

```sh
$ git remote add heroku https://git.heroku.com/herxheim2018.git
$ git push heroku master
```
