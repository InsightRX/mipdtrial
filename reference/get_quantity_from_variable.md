# Get quantities from variables in sim results

Used to add quantities to rows in advice table, exposure time table.

## Usage

``` r
get_quantity_from_variable(var, sim, md, times, comp = NULL)
```

## Arguments

- var:

  Name of variable

- sim:

  Output of PKPDsim::sim_ode()

- md:

  Metadata object

- times:

  Times at which to calculate

- comp:

  if not NULL, filter by `sim$comp`
