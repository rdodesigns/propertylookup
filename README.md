Property Lookup for Boulder CO
==============================

Boulder County has a system for looking up public records on land (such as
owners, structures on the land, square footage of said structures, etc).
However, the lookup system in its current iteration (as of this writing) is
built using SilverLight, and is not as amenable to scraping.

This (small) program looks up a given list of property records, tabulating
the total square footage as well as creating a PDF copy of the record for
record keeping (for the property owners/lawyers).


How to use
==========

The interface is simple. Just provide a list of property record numbers in a
file, one per line (like if one were to copy out of Excel) and provide that
to the program.

```
$ propertylookup example_records.txt output_directory
```

Requirements
============

- Haskell :-)
- [wkhtmltopdf](https://github.com/wkhtmltopdf/wkhtmltopdf), which converts
  HTML to PDFs using the webkit engine.
