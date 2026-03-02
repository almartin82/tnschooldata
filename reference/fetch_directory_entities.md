# Fetch directory entities from the TN School Directory API

Queries the FetchXML endpoint for a specific entity type (school or
district) with pagination support. Includes a linked join to the contact
entity to retrieve administrator names and emails.

## Usage

``` r
fetch_directory_entities(entity_type, page_size = 5000)
```

## Arguments

- entity_type:

  The customertypecode value: "100000001" for schools, "100000002" for
  districts

- page_size:

  Number of records per page (max 5000)

## Value

Data frame with all records for the entity type
