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
$ env PORT=8080 SLEEPING_LIMIT=120 CAMPING_LIMIT=50 ADMIN_PASSWORD=admin DATABASE_URL='postgres://localhost/herxheim2018' stack exec herxheim2018-exe
```

### Environment variables

The following environment variables need to be set:

* `DATABASE_URL` (string): A PostgreSQL database connection string
* `PORT` (number): The port the web server should run on
* `ADMIN_PASSWORD` (string): The password for the _HTTP basic authentication_ protected admin area
* `PARTICIPANT_LIMIT` (number): The maximum number of participants allowed before closing sleepover registration

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
