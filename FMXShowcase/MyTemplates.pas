unit MyTemplates;

interface

const PlainTemplate1 = '''
  Name: @person.name
  Age: @person.age

  @if person.AgeBelowTen {
  @person.name is BELOW ten.
  }  @else {
  @person.name is OVER ten.
  }
  ''';

const HTMLTemplate1 = '''
  <style>
    body {
      background-color: white !important;
      color: black !important;
    }
  </style>
  <ul>
    <li>Name: <strong>@person.name</strong></li>
    <li>Age: <strong>@person.age</strong></li>
  </ul>

  @if person.AgeBelowTen {
  <p>@person.name is <strong>BELOW</strong> ten.</p>
  }  @else {
  <p>@person.name is <strong>OVER</strong> ten.</p>
  }
  ''';

implementation

end.
