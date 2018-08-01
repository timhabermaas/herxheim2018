# Herxheim2018

## Installation

```sh
$ stack setup
$ stack build
```

## Run the server

```sh
$ env PORT=8080 ADMIN_PASSWORD=admin DATABASE_URL='postgres://localhost/herxheim2018' stack exec herxheim2018-exe
```

## Deployment

The application is currently deployed to Heroku. For deployment run:

```sh
$ git remote add heroku https://git.heroku.com/herxheim2018.git
$ git push heroku master
```
