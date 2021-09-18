#include <array>
#include <cassert>
#include <composition_helper.hpp>
#include <cstdint>
#include <vector>

#define EXPECTED_RESULT(type, count_, ...)                     \
    auto results = std::array< type, count_ > { __VA_ARGS__ }; \
    auto count   = 0;

#define CHECK_RESULT(data, function)                 \
    for (auto element : data) {                      \
        assert(function(element) == results[count]); \
        count++;                                     \
    }                                                \
    assert(count == results.size());

#define CHECK_ELEMENT_RESULT(data, function, member)        \
    for (auto element : data) {                             \
        assert(function(element).member == results[count]); \
        count++;                                            \
    }                                                       \
    assert(count == results.size());

double add(std::pair< double, double > x) noexcept {
    return x.first + x.second;
}
double square(double x) noexcept {
    return x * x;
}
double subtract_ten(double x) noexcept {
    return x - 10.;
}

void test_function_composition() {
    auto my_data = std::vector< std::pair< double, double > > { { 3, 1 }, { 5, 1 }, { 7, 1 }, { 8, 1 } };
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71)

    auto F = fp::CompositionFunction { add }.Compose(square).Compose(subtract_ten);

    CHECK_RESULT(my_data, F);
}

static auto lambda_add_one       = [](int x) { return x + 1; };
static auto lambda_int_to_double = [](int x) { return static_cast< double >(x); };
static auto lambda_square        = [](double x) { return x * x; };
static auto lambda_subtract_ten  = [](double x) { return x - 10.; };

void test_lambda_composition() {
    auto my_data = std::vector< int > { 3, 5, 7, 8 };
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);

    auto F = fp::CompositionFunction { lambda_add_one }.Compose(lambda_int_to_double).Compose(lambda_square).Compose(lambda_subtract_ten);

    CHECK_RESULT(my_data, F);
}

void test_free_compose() {
    auto my_data = std::vector< int > { 3, 5, 7, 8 };
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);

    auto F = fp::Compose(fp::Compose(lambda_add_one, lambda_int_to_double), lambda_square).Compose(lambda_subtract_ten);

    CHECK_RESULT(my_data, F);
}

enum class IngredientType : std::uint8_t
{
    Flour = 1,
    Salad = 2,
    Meat  = 3,

    Undefined = std::numeric_limits< std::uint8_t >::max(),
};

struct Ingredient {
    IngredientType type = IngredientType::Undefined;
};
struct Cost {
    int cost = 0;
};
struct Food {
    int food = 0;
};
struct Calories {
    int calories = 0;
};
struct Weight {
    int weight = 0;
};

Cost calcCost(Ingredient ing) {
    return Cost { static_cast< std::uint8_t >(ing.type) * 10 };
}
struct Buyer {
    Food operator()(Cost c) const {
        return Food { c.cost / 10 };
    }
};
static auto lambda_cook_food = [](Food f) { return Calories { (f.food % 2) * 1000 }; };
struct Scale {
    Weight operator()(Calories c) {
        return Weight { c.calories / 10 + 50 };
    }
};

void test_combination() {
    auto my_data = std::vector< Ingredient > { { IngredientType::Flour }, { IngredientType::Salad }, { IngredientType::Meat } };
    EXPECTED_RESULT(int, 3, 150, 50, 150);

    auto F = fp::CompositionFunction { calcCost }.Compose(Buyer {}).Compose(lambda_cook_food).Compose(Scale {});

    CHECK_ELEMENT_RESULT(my_data, F, weight);
}

constexpr double add_constexpr(std::pair< double, double > x) noexcept {
    return x.first + x.second;
}
constexpr double square_constexpr(double x) noexcept {
    return x * x;
}
constexpr double subtract_ten_constexpr(double x) noexcept {
    return x - 10.;
}

constexpr bool test_function_composition_constexpr() {
#ifdef _MSC_VER
    std::vector< std::pair< double, double > > my_data { { 3, 1 }, { 5, 1 }, { 7, 1 }, { 8, 1 } };
    auto                                       nElements = my_data.size();
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);
#else  // _MSC_VER
    std::pair< double, double > my_data[4] = { { 3, 1 }, { 5, 1 }, { 7, 1 }, { 8, 1 } };
    auto                        nElements  = 4;
    double                      results[4] = { 6, 26, 54, 71 };
    auto                        count      = 0;
#endif // !_MSC_VER

    constexpr auto F = fp::CompositionFunction { add_constexpr }.Compose(square_constexpr).Compose(subtract_ten_constexpr);
    static_assert(std::is_nothrow_invocable_v< decltype(F), std::pair< double, double > >);

    for (auto& element : my_data) {

        if (F(element) != results[count]) {
            return false;
        }
        count++;
    }
    if (count != nElements) {
        return false;
    }
    return true;
}
static_assert((test_function_composition_constexpr(), true));

static auto lambda_add_one_constexpr = [](int x) constexpr noexcept {
    return x + 1;
};
static auto lambda_int_to_double_constexpr = [](int x) constexpr noexcept {
    return static_cast< double >(x);
};
static auto lambda_square_constexpr = [](double x) constexpr {
    return x * x;
};
static auto lambda_subtract_ten_constexpr = [](double x) constexpr noexcept {
    return x - 10.;
};

constexpr bool test_lambda_composition_constexpr() {
#ifdef _MSC_VER
    auto my_data   = std::vector< int > { 3, 5, 7, 8 };
    auto nElements = my_data.size();
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);
#else  // _MSC_VER
    int                         my_data[4] = { 3, 5, 7, 8 };
    auto                        nElements  = 4;
    double                      results[4] = { 6, 26, 54, 71 };
    auto                        count      = 0;
#endif // !_MSC_VER

    constexpr auto F = fp::CompositionFunction { lambda_add_one_constexpr }
                           .Compose(lambda_int_to_double_constexpr)
                           .Compose(lambda_square_constexpr)
                           .Compose(lambda_subtract_ten_constexpr);
    static_assert(!std::is_nothrow_invocable_v< decltype(F), int >);

    for (auto& element : my_data) {

        if (F(element) != results[count]) {
            return false;
        }
        count++;
    }
    if (count != nElements) {
        return false;
    }
    return true;
}
static_assert((test_lambda_composition_constexpr(), true));

constexpr Cost constexpr_calcCost(Ingredient ing) {
    return Cost { static_cast< std::uint8_t >(ing.type) * 10 };
}
struct constexpr_Buyer {
    constexpr Food operator()(Cost c) const {
        return Food { c.cost / 10 };
    }
};
static auto constexpr_lambda_cook_food = [](Food f) constexpr {
    return Calories { (f.food % 2) * 1000 };
};
struct constexpr_Scale {
    constexpr Weight operator()(Calories c) const {
        return Weight { c.calories / 10 + 50 };
    }
};

constexpr bool test_callable_struct_constexpr() {
#ifdef _MSC_VER
    auto my_data = std::vector< Ingredient > { { IngredientType::Flour }, { IngredientType::Salad }, { IngredientType::Meat } };
    EXPECTED_RESULT(int, 3, 150, 50, 150);
    auto nElements = my_data.size();
#else  // _MSC_VER
    Ingredient                  my_data[3] = { { IngredientType::Flour }, { IngredientType::Salad }, { IngredientType::Meat } };
    auto                        nElements  = 3;
    int                         results[3] = { 150, 50, 150 };
    auto                        count      = 0;
#endif // !_MSC_VER

    constexpr auto F = fp::CompositionFunction { constexpr_calcCost }.Compose(constexpr_Buyer {}).Compose(constexpr_lambda_cook_food).Compose(constexpr_Scale {});

    for (auto& element : my_data) {

        if (F(element).weight != results[count]) {
            return false;
        }
        count++;
    }
    if (count != nElements) {
        return false;
    }
    return true;
}
static_assert((test_callable_struct_constexpr(), true));

static auto f_noexcept         = [](int x) noexcept { return x; };
static auto f_noexcept_2       = [](int x) noexcept { return x; };
static auto f_noexcept_mutable = [](int x) mutable noexcept { return x; };
static auto f_except           = [](int x) { return x; };
static auto f_except_mutable   = [](int x) mutable { return x; };
static auto f_except_mutable_2 = [](int x) mutable { return x; };

struct s_const_noexcept {
    int operator()(int x) const noexcept {
        return x;
    }
};
struct s_const {
    int operator()(int x) const {
        return x;
    }
};
struct s_noexcept {
    int operator()(int x) noexcept {
        return x;
    }
};
struct s_none {
    int operator()(int x) {
        return x;
    }
};

void test_traits() {

    {
        // Traits
        static_assert(std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { add }.Compose(square)), std::pair< double, double > >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { calcCost }.Compose(Buyer {})), Ingredient >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::Compose(lambda_add_one, lambda_int_to_double)), Ingredient >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { calcCost }.Compose(Buyer {})), Ingredient >);
        static_assert(!std::is_invocable_v< decltype(fp::CompositionFunction { calcCost }.Compose(Buyer {})), std::pair< double, double > >);
        static_assert(std::is_invocable_v< decltype(fp::Compose(lambda_add_one, lambda_int_to_double)), int >);
        static_assert(!std::is_invocable_v< decltype(fp::Compose(lambda_add_one, lambda_int_to_double)), Ingredient >);

        static_assert(std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_const {} }), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_const {} }), int >);
        static_assert(std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_noexcept {} }), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_none {} }), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_none {} }), int >);

        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_const {})), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_const {})), int >);
        static_assert(std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_noexcept {})), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_none {})), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_none {})), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_const {}).Compose(s_noexcept {})), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_const {}).Compose(s_noexcept {})), int >);

        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const {})), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const {})), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_none {} }.Compose(s_noexcept {})), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_none {} }.Compose(s_noexcept {})), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const_noexcept {})), int >);
        static_assert(std::is_invocable_v< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const_noexcept {})), int >);
    }
    {
        // testing Using functor_traits
        static_assert(fp::functor_traits< decltype(fp::Compose(lambda_add_one, lambda_int_to_double)) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::Compose(lambda_add_one, lambda_int_to_double)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::Compose(f_noexcept, f_noexcept_mutable)) >::has_const);
        static_assert(fp::functor_traits< decltype(fp::Compose(f_noexcept, f_noexcept_mutable)) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::Compose(f_noexcept, f_except)) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::Compose(f_noexcept, f_except)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::Compose(f_except, f_except_mutable)) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::Compose(f_except, f_except_mutable)) >::has_noexcept);

        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { add }) >::has_const);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { add }.Compose(square)) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { add }.Compose(square)) >::has_const);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { Buyer {} }) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { Buyer {} }) >::has_noexcept);

        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { s_const_noexcept {} }) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { s_const_noexcept {} }) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_const {} }) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { s_const {} }) >::has_const);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { s_noexcept {} }) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_noexcept {} }) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }) >::has_const);

        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction< s_const_noexcept > { s_const_noexcept {} }.Compose(s_const {})) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction< s_const_noexcept > { s_const_noexcept {} }.Compose(s_const {})) >::has_const);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction< s_const_noexcept > { s_const_noexcept {} }.Compose(s_noexcept {})) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction< s_const_noexcept > { s_const_noexcept {} }.Compose(s_noexcept {})) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction< s_const_noexcept > { s_const_noexcept {} }.Compose(s_none {})) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction< s_const_noexcept > { s_const_noexcept {} }.Compose(s_none {})) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_const {}).Compose(s_noexcept {})) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_const_noexcept {} }.Compose(s_const {}).Compose(s_noexcept {})) >::has_const);

        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const {})) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const {})) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }.Compose(s_noexcept {})) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }.Compose(s_noexcept {})) >::has_const);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const_noexcept {})) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { s_none {} }.Compose(s_const_noexcept {})) >::has_const);
    }
    {
        // Traits
        static_assert(std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { f_noexcept }), int >);
        static_assert(std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_noexcept_2)), int >);
        static_assert(std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_noexcept_2).Compose(f_noexcept_mutable)), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { f_except }), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_except)), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_except).Compose(f_noexcept_2)), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(fp::CompositionFunction { f_except }.Compose(f_noexcept)), int >);
    }
    {
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_noexcept_2)) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_noexcept_2).Compose(f_noexcept_mutable)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_except }) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_except)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_except).Compose(f_noexcept_2)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_except }.Compose(f_noexcept)) >::has_noexcept);
    }
    {
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_noexcept_2)) >::has_noexcept);
        static_assert(fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_noexcept_2).Compose(f_noexcept_mutable)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_except }) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_except)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_noexcept }.Compose(f_except).Compose(f_noexcept_2)) >::has_noexcept);
        static_assert(!fp::functor_traits< decltype(fp::CompositionFunction { f_except }.Compose(f_noexcept)) >::has_noexcept);
    }
}

int main() {
    test_traits();
    test_function_composition();
    test_lambda_composition();
    test_free_compose();
    test_combination();

    assert(test_function_composition_constexpr() == true);
    assert(test_lambda_composition_constexpr() == true);
    assert(test_callable_struct_constexpr() == true);
}
