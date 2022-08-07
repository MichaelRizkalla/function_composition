# function composition helper
 A header only library that aims to help in function composition procedure.

# Getting started
This code requires C++20 compliant compiler.
You can copy the header in your project, include and use it.

# Use example
```C++
#include <composition_function.hpp>
#include <cassert>

auto add_one = [](int x) noexcept { return x + 1; };
auto subtract_one = [](int x) noexcept { return x - 1; };

auto identity = mr::composition_function { add_one } | subtract_one; // subtract_one(add_one(Args...))
/*
Or 
auto identity = mr::composition_function { add_one }.Compose(subtract_one);
Or 
auto identity = mr::compose | add_one | subtract_one;
*/

int value = 10;
assert(identity(value) == value);
```

# License
This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details
