# Build FetchXML query for directory data

Constructs a FetchXML query string for the TN School Directory API.
Includes attributes for school/district details and a linked entity join
to get primary contact (principal/superintendent) information.

## Usage

``` r
build_directory_fetchxml(
  entity_type,
  page_size = 5000,
  page_number = 1,
  paging_cookie = NULL
)
```

## Arguments

- entity_type:

  The customertypecode value

- page_size:

  Number of records per page

- page_number:

  Current page number

- paging_cookie:

  Paging cookie from previous response (NULL for first page)

## Value

FetchXML string
