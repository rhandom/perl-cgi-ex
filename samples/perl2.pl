### this yaml will return an arrayref containing three hashrefs
### this shows three groups
### the first two groups have validate_if's
[
   {
     'group validate_if' => 'foo',
     bar => {
       required => 1,
     },
   },
   {
     'group validate_if' => 'hem',
     haw => { required => 1 },
   },
   {
     raspberry => {
       required => 1,
     },
   },
];
