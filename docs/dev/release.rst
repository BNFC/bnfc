=========
Releasing
=========

☐ Write changes
    In `source/changelog`. Write an overview of the important changes: features
    added, deprecated and removed.

    The format of the first line is::

      2.8 Grégoire Détrez <gregoire@fripost.org> March 2042
      ├┄┘ └┄┄┄┄┄┄┄┄┄┄┄┄┬┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┘ ├┄┄┄┄┄┄┄┄┘
      ┕ version number ┕ Releaser name and email ┕ Date

☐ Bumb version
    The version increment should be based on the changes listed in the
    changelog, according to `semantic versioning`_.

☐ Run the tests 🅰
    Run all the tests suites (or let the CI server do it).

☐ Build binaries 🅰
    Build binaries for mac, windows and linux (or use the ones build by the CI
    server).

☐ Create a new release on github
    A github release includes:

    - the commit hash of the code revision
    - the version number
    - the list of changes from the changelog
    - the source package
    - the binary packages

☐ Upload the source package on hackage
    https://hackage.haskell.org/upload

☐ Activate readthedocs for the new version
    This is done at https://readthedocs.org/projects/bnfc/versions/

.. _semantic versioning:
   http://semver.org/
