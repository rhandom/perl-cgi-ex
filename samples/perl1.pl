### this file is very simplistic
### but it shows how easy the file can be
{
  user => {
    required => 1,
  },
  foo => {
    required_if => 'bar',
  },
}
# last item returned must be the ref
