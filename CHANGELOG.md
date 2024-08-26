# Changelog [![Elm package](https://img.shields.io/elm-package/v/dillonkearns/elm-form.svg)](https://package.elm-lang.org/packages/dillonkearns/elm-form/latest/)

All notable changes to
[the `dillonkearns/elm-form` elm package](http://package.elm-lang.org/packages/dillonkearns/elm-form/latest)
will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to
[Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [3.0.1] - 2024-08-26

- Fix crash in case where status is compared for global field, fixes [#16](https://github.com/dillonkearns/elm-form/issues/16).

## [2.0.2] - 2023-04-18

### Fixed

- Fixed min/max validations (see [#1](https://github.com/dillonkearns/elm-form/issues/1)).

## [2.0.1] - 2023-04-17

### Added

- Changed README to include an Ellie demo.

## [2.0.0] - 2023-04-17

### Fixed

- The type variable for `error` was mistakenly hardcoded as `String` in the `Form.form` function. This allows the `error` type to be any value (though it is usually a `String`).
