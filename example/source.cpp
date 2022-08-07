#include <algorithm>
#include <array>
#include <cassert>
#include <composition_function.hpp>
#include <cstdint>
#include <functional>
#include <limits>
#include <string_view>
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

    auto F = (mr::compose | add | square).Compose(subtract_ten);

    CHECK_RESULT(my_data, F);
}

static auto lambda_add_one       = [](int x) { return x + 1; };
static auto lambda_int_to_double = [](int x) { return static_cast< double >(x); };
static auto lambda_square        = [](double x) { return x * x; };
static auto lambda_subtract_ten  = [](double x) { return x - 10.; };

void test_lambda_composition() {
    auto my_data = std::vector< int > { 3, 5, 7, 8 };
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);

    auto F =
        mr::compose(lambda_add_one).Compose(lambda_int_to_double).Compose(lambda_square).Compose(lambda_subtract_ten);

    CHECK_RESULT(my_data, F);
}

void test_free_compose() {
    auto my_data = std::vector< int > { 3, 5, 7, 8 };
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);

    auto F = mr::compose(lambda_add_one, lambda_int_to_double) | lambda_square | lambda_subtract_ten;

    CHECK_RESULT(my_data, F);
}

enum class IngredientType : std::uint8_t {
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
    auto my_data =
        std::vector< Ingredient > { { IngredientType::Flour }, { IngredientType::Salad }, { IngredientType::Meat } };
    EXPECTED_RESULT(int, 3, 150, 50, 150);

    auto F = mr::compose | calcCost | Buyer {} | lambda_cook_food | Scale {};

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
#ifndef __clang__
    std::vector< std::pair< double, double > > my_data { { 3, 1 }, { 5, 1 }, { 7, 1 }, { 8, 1 } };
    auto                                       nElements = my_data.size();
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);
#else  // !__clang__
    std::pair< double, double > my_data[4] = { { 3, 1 }, { 5, 1 }, { 7, 1 }, { 8, 1 } };
    auto                        nElements  = 4;
    double                      results[4] = { 6, 26, 54, 71 };
    auto                        count      = 0;
#endif // __clang__

    constexpr auto F =
        mr::composition_function { add_constexpr }.Compose(square_constexpr).Compose(subtract_ten_constexpr);
    static_assert(std::is_nothrow_invocable_v< decltype(F), std::pair< double, double > >);

    for (auto&& element : my_data) {

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
// static_assert((test_function_composition_constexpr(), true));

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
#ifndef __clang__
    auto my_data   = std::vector< int > { 3, 5, 7, 8 };
    auto nElements = my_data.size();
    EXPECTED_RESULT(double, 4, 6, 26, 54, 71);
#else  // !__clang__
    int                         my_data[4] = { 3, 5, 7, 8 };
    auto                        nElements  = 4;
    double                      results[4] = { 6, 26, 54, 71 };
    auto                        count      = 0;
#endif // __clang__

#ifndef _MSC_VER
    constexpr auto F = mr::compose(lambda_add_one_constexpr)
                           .Compose(lambda_int_to_double_constexpr)
                           .Compose(lambda_square_constexpr)
                           .Compose(lambda_subtract_ten_constexpr);
    static_assert(!std::is_nothrow_invocable_v< decltype(F), int >);

    for (auto element : my_data) {

        if (F(element) != results[count]) {
            return false;
        }
        count++;
    }
    if (count != nElements) {
        return false;
    }
#endif //_MSC_VER
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
#ifndef __clang__
    auto my_data =
        std::vector< Ingredient > { { IngredientType::Flour }, { IngredientType::Salad }, { IngredientType::Meat } };
    EXPECTED_RESULT(int, 3, 150, 50, 150);
    auto nElements = my_data.size();
#else  // !__clang__
    Ingredient my_data[3] = { { IngredientType::Flour }, { IngredientType::Salad }, { IngredientType::Meat } };
    auto       nElements  = 3;
    int        results[3] = { 150, 50, 150 };
    auto       count      = 0;
#endif // __clang__

#ifndef _MSC_VER
    constexpr auto F =
        mr::compose | constexpr_calcCost | constexpr_Buyer {} | constexpr_lambda_cook_food | constexpr_Scale {};

    for (auto& element : my_data) {

        if (F(element).weight != results[count]) {
            return false;
        }
        count++;
    }
    if (count != nElements) {
        return false;
    }
#endif // _MSC_VER
    return true;
}
// static_assert((test_callable_struct_constexpr(), true));

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
    { // Traits
        static_assert(std::is_nothrow_invocable_v< decltype(mr::compose(add) | square), std::pair< double, double > >);
        static_assert(!std::is_nothrow_invocable_v< decltype(mr::compose | calcCost | Buyer {}), Ingredient >);
        static_assert(
            !std::is_nothrow_invocable_v< decltype(mr::compose(lambda_add_one, lambda_int_to_double)), Ingredient >);
        static_assert(std::is_invocable_v< decltype(mr::compose | calcCost | Buyer {}), Ingredient >);
        static_assert(!std::is_invocable_v< decltype(mr::composition_function { calcCost, Buyer {} }),
                                            std::pair< double, double > >);
        static_assert(
            std::is_invocable_v< decltype(mr::composition_function(lambda_add_one, lambda_int_to_double)), int >);
        static_assert(!std::is_invocable_v< decltype(mr::composition_function(lambda_add_one, lambda_int_to_double)),
                                            Ingredient >);

        static_assert(std::is_nothrow_invocable_v< decltype(mr::compose(s_const_noexcept {})), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(mr::composition_function { s_const {} }), int >);
        static_assert(std::is_invocable_v< decltype(mr::composition_function { s_const {} }), int >);
        static_assert(std::is_nothrow_invocable_v< decltype(mr::composition_function { s_noexcept {} }), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(mr::composition_function { s_none {} }), int >);
        static_assert(std::is_invocable_v< decltype(mr::composition_function { s_none {} }), int >);

        static_assert(!std::is_nothrow_invocable_v<
                      decltype(mr::composition_function { s_const_noexcept {} }.Compose(s_const {})), int >);
        static_assert(
            std::is_invocable_v< decltype(mr::composition_function { s_const_noexcept {} } | s_const {}), int >);
        static_assert(std::is_nothrow_invocable_v<
                      decltype(mr::composition_function { s_const_noexcept {} }.Compose(s_noexcept {})), int >);
        static_assert(
            !std::is_nothrow_invocable_v< decltype(mr::composition_function { s_const_noexcept {} }.Compose(s_none {})),
                                          int >);
        static_assert(
            std::is_invocable_v< decltype(mr::composition_function { s_const_noexcept {} }.Compose(s_none {})), int >);
        static_assert(
            !std::is_nothrow_invocable_v<
                decltype(mr::composition_function { s_const_noexcept {} }.Compose(s_const {}).Compose(s_noexcept {})),
                int >);
        static_assert(
            std::is_invocable_v<
                decltype(mr::composition_function { s_const_noexcept {} }.Compose(s_const {}).Compose(s_noexcept {})),
                int >);

        static_assert(
            !std::is_nothrow_invocable_v< decltype(mr::composition_function { s_none {} }.Compose(s_const {})), int >);
        static_assert(std::is_invocable_v< decltype(mr::composition_function { s_none {} }.Compose(s_const {})), int >);
        static_assert(
            !std::is_nothrow_invocable_v< decltype(mr::composition_function { s_none {} }.Compose(s_noexcept {})),
                                          int >);
        static_assert(
            std::is_invocable_v< decltype(mr::composition_function { s_none {} }.Compose(s_noexcept {})), int >);
        static_assert(
            !std::is_nothrow_invocable_v< decltype(mr::composition_function { s_none {} }.Compose(s_const_noexcept {})),
                                          int >);
        static_assert(
            std::is_invocable_v< decltype(mr::composition_function { s_none {} }.Compose(s_const_noexcept {})), int >);
    }
    {
        static_assert(!mr::mr_traits::callable_traits< decltype(mr::compose(lambda_add_one,
                                                                            lambda_int_to_double)) >::is_noexcept);
        static_assert(
            mr::mr_traits::callable_traits< decltype(mr::compose(f_noexcept, f_noexcept_mutable)) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(mr::compose(f_noexcept, f_except)) >::is_noexcept);
        static_assert(
            !mr::mr_traits::callable_traits< decltype(mr::compose(f_except, f_except_mutable)) >::is_noexcept);
        static_assert(
            mr::mr_traits::callable_traits< decltype(mr::composition_function { add }.Compose(square)) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(mr::composition_function { Buyer {} }) >::is_noexcept);
        static_assert(
            mr::mr_traits::callable_traits< decltype(mr::composition_function { s_const_noexcept {} }) >::is_noexcept);
        static_assert(
            !mr::mr_traits::callable_traits< decltype(mr::composition_function { s_const {} }) >::is_noexcept);
        static_assert(
            mr::mr_traits::callable_traits< decltype(mr::composition_function { s_noexcept {} }) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(mr::composition_function { s_none {} }) >::is_noexcept);

        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { s_const_noexcept {} }.Compose(s_const {})) >::is_noexcept);
        static_assert(mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { s_const_noexcept {} }.Compose(s_noexcept {})) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { s_const_noexcept {} }.Compose(s_none {})) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(mr::composition_function { s_const_noexcept {} }
                                                                    .Compose(s_const {})
                                                                    .Compose(s_noexcept {})) >::is_noexcept);

        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { s_none {} }.Compose(s_const {})) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { s_none {} }.Compose(s_noexcept {})) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { s_none {} }.Compose(s_const_noexcept {})) >::is_noexcept);
    }
    {
        // Traits
        static_assert(std::is_nothrow_invocable_v< decltype(mr::composition_function { f_noexcept }), int >);
        static_assert(
            std::is_nothrow_invocable_v< decltype(mr::composition_function { f_noexcept }.Compose(f_noexcept_2)),
                                         int >);
        static_assert(
            std::is_nothrow_invocable_v<
                decltype(mr::composition_function { f_noexcept }.Compose(f_noexcept_2) | (f_noexcept_mutable)), int >);
        static_assert(!std::is_nothrow_invocable_v< decltype(mr::composition_function { f_except }), int >);
        static_assert(
            !std::is_nothrow_invocable_v< decltype(mr::composition_function { f_noexcept }.Compose(f_except)), int >);
        static_assert(!std::is_nothrow_invocable_v<
                      decltype((mr::composition_function { f_noexcept } | f_except).Compose(f_noexcept_2)), int >);
        static_assert(
            !std::is_nothrow_invocable_v< decltype(mr::composition_function { f_except }.Compose(f_noexcept)), int >);
    }
    {
        static_assert(mr::mr_traits::callable_traits< decltype(mr::composition_function { f_noexcept }) >::is_noexcept);
        static_assert(mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { f_noexcept }.Compose(f_noexcept_2)) >::is_noexcept);
        static_assert(mr::mr_traits::callable_traits< decltype(mr::composition_function { f_noexcept }
                                                                   .Compose(f_noexcept_2)
                                                                   .Compose(f_noexcept_mutable)) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(mr::composition_function { f_except }) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { f_noexcept }.Compose(f_except)) >::is_noexcept);
        static_assert(
            !mr::mr_traits::callable_traits< decltype(
                mr::composition_function { f_noexcept }.Compose(f_except).Compose(f_noexcept_2)) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { f_except }.Compose(f_noexcept)) >::is_noexcept);
    }
    {
        static_assert(mr::mr_traits::callable_traits< decltype(mr::composition_function { f_noexcept }) >::is_noexcept);
        static_assert(mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { f_noexcept }.Compose(f_noexcept_2)) >::is_noexcept);
        static_assert(
            mr::mr_traits::callable_traits< decltype(mr::composition_function { f_noexcept }.Compose(f_noexcept_2) |
                                                     (f_noexcept_mutable)) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(mr::composition_function { f_except }) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { f_noexcept }.Compose(f_except)) >::is_noexcept);
        static_assert(
            !mr::mr_traits::callable_traits< decltype(
                mr::composition_function { f_noexcept }.Compose(f_except).Compose(f_noexcept_2)) >::is_noexcept);
        static_assert(!mr::mr_traits::callable_traits< decltype(
                          mr::composition_function { f_except }.Compose(f_noexcept)) >::is_noexcept);
    }
}
void test_composable_trait() {
    static_assert(mr::mr_traits::composable< s_const, s_const_noexcept >);
    static_assert(mr::mr_traits::composable< s_const, s_none >);
    static_assert(mr::mr_traits::composable< decltype(square), decltype(square) >);
    static_assert(mr::mr_traits::composable< decltype(f_noexcept), decltype(f_except_mutable) >);
    static_assert(mr::mr_traits::composable< decltype(add_constexpr), decltype(square_constexpr) >);
    static_assert(mr::mr_traits::composable< decltype(lambda_cook_food), Scale >);
    static_assert(mr::mr_traits::composable< decltype(calcCost), Buyer >);
    static_assert(mr::mr_traits::composable< decltype(square), decltype(lambda_square) >);

    static_assert(!mr::mr_traits::composable< s_const, decltype(square) >);
    static_assert(!mr::mr_traits::composable< Scale, Buyer >);
}

namespace utility {
    struct Data {
        int copies;
        int moves;
        int assign_copies;
        int assign_moves;
        int objects_alive;
    };
    struct TrackedCallable {
        TrackedCallable() noexcept : Alive(true) {
            ++objects_alive;
        }
        ~TrackedCallable() noexcept {
            if (Alive)
                --objects_alive;
        }
        TrackedCallable(const TrackedCallable& rhs) noexcept : Alive(rhs.Alive) {
            if (Alive)
                ++objects_alive;
            ++copies;
        }
        TrackedCallable(TrackedCallable&& rhs) noexcept : Alive(std::exchange(rhs.Alive, false)) {
            ++moves;
        }
        TrackedCallable& operator=(const TrackedCallable& rhs) noexcept {
            Alive = rhs.Alive;
            if (Alive)
                ++objects_alive;
            ++assign_copies;
            return *this;
        }
        TrackedCallable& operator=(TrackedCallable&& rhs) noexcept {
            Alive = std::exchange(rhs.Alive, false);
            ++assign_moves;
            return *this;
        }
        Data operator()() const noexcept {
            return { .copies        = copies,
                     .moves         = moves,
                     .assign_copies = assign_copies,
                     .assign_moves  = assign_moves,
                     .objects_alive = objects_alive };
        }

        static void reset() noexcept {
            moves         = 0;
            copies        = 0;
            assign_moves  = 0;
            assign_copies = 0;
        }

        bool Alive = 0;

        inline static std::uint8_t moves         = 0;
        inline static std::uint8_t copies        = 0;
        inline static std::uint8_t assign_moves  = 0;
        inline static std::uint8_t assign_copies = 0;
        inline static std::uint8_t objects_alive = 0;
    };
} // namespace utility
void test_construction() {
    auto passthrough = [](utility::Data x) noexcept { return std::forward< decltype(x) >(x); };
    {
        utility::TrackedCallable f {};

        auto function = mr::compose | std::move(f) /*move*/ | passthrough;
        static_assert(std::is_invocable_r_v< utility::Data, decltype(function) >);
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        assert(utility::TrackedCallable::objects_alive == 1);
        assert(utility::TrackedCallable::moves == 2);
        assert(utility::TrackedCallable::copies == 0);
        assert(utility::TrackedCallable::assign_moves == 0);
        assert(utility::TrackedCallable::assign_copies == 0);

        const auto data = function();
        assert(utility::TrackedCallable::objects_alive == data.objects_alive);
        assert(utility::TrackedCallable::moves == data.moves);
        assert(utility::TrackedCallable::copies == data.copies);
        assert(utility::TrackedCallable::objects_alive == 1);
        assert(utility::TrackedCallable::moves == 2);
        assert(utility::TrackedCallable::copies == 0);
        assert(utility::TrackedCallable::assign_moves == 0);
        assert(utility::TrackedCallable::assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        utility::TrackedCallable f {};
        auto                     function = mr::compose | f /*copy*/ | passthrough;
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 2);
        assert(data.moves == 1);
        assert(data.copies == 1);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        utility::TrackedCallable f {};
        auto                     function = mr::composition_function { f /*copy*/, passthrough };
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 2);
        assert(data.moves == 0);
        assert(data.copies == 1);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        const utility::TrackedCallable f {};
        auto                           function = mr::compose | f | passthrough;
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 2);
        assert(data.moves == 1);
        assert(data.copies == 1);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        const utility::TrackedCallable f {};
        auto                           function = mr::composition_function { f, passthrough };
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 2);
        assert(data.moves == 0);
        assert(data.copies == 1);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        auto function = mr::composition_function { utility::TrackedCallable {}, passthrough };
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 1);
        assert(data.moves == 1);
        assert(data.copies == 0);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        auto function = mr::compose | utility::TrackedCallable {} | passthrough;
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 1);
        assert(data.moves == 2);
        assert(data.copies == 0);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        auto function =
            mr::compose | static_cast< const utility::TrackedCallable&& >(utility::TrackedCallable {}) | passthrough;
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 1);
        assert(data.moves == 1);
        assert(data.copies == 1);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
    {
        auto function =
            mr::composition_function { static_cast< const utility::TrackedCallable&& >(utility::TrackedCallable {}),
                                       passthrough };
        static_assert(std::is_nothrow_invocable_r_v< utility::Data, decltype(function) >);
        const auto data = function();
        assert(data.objects_alive == 1);
        assert(data.moves == 0);
        assert(data.copies == 1);
        assert(data.assign_moves == 0);
        assert(data.assign_copies == 0);
    }
    utility::TrackedCallable::reset();
}

constexpr void test_1() {
    using namespace std::literals::string_view_literals;
    struct Args {
        std::string_view str;
        std::string_view options;
    };
    constexpr auto str = "This is a test string, which will be searched for a specific sub string!"sv;

    constexpr auto get_string = [str](std::string_view options) constexpr noexcept {
        return Args { str, options };
    };
    constexpr auto find_first =
#ifndef __clang__
        [](Args args) constexpr noexcept(noexcept(std::ranges::search(args.str, args.options))) {
#else  // __clang__
        [](Args args) constexpr noexcept(
            noexcept(std::search(args.str.begin(), args.str.end(), args.options.begin(), args.options.end()))) {
#endif // !__clang__
        auto&& [str, options] = args;
        if (options.empty()) {
            return ""sv;
        }

#ifndef __clang__
        auto result = std::ranges::search(str, options);
        return std::string_view { result.begin(), str.end() };
#else  // __clang__
        auto result = std::search(args.str.begin(), args.str.end(), args.options.begin(), args.options.end());
        return std::string_view { result, str.end() };
#endif // !__clang__
    };
    constexpr auto count_chars = [](std::string_view str) constexpr noexcept {
        return str.size();
    };

    constexpr auto operation       = mr::compose | get_string | find_first;
    constexpr auto count_operation = operation | count_chars;
    {
        constexpr auto option = "specific"sv;
        constexpr auto result = operation(option);
        static_assert(result == "specific sub string!"sv);
        static_assert(count_operation(option) == 20);
    }
    {
        constexpr auto option = "sub"sv;
        constexpr auto result = operation(option);
        static_assert(result == "sub string!"sv);
        static_assert(count_operation(option) == 11);
    }
}
static_assert((test_1(), true));

void test_2() {
    using namespace std::literals::string_view_literals;
    struct Args {
        std::string_view str;
        std::string_view options;
    };
    constexpr auto str = "This is a test string, which will be searched for a specific sub string!"sv;

    std::function get_string = [str](std::string_view options) noexcept { return Args { str, options }; };

#ifndef __clang__
    std::function find_first = [](Args args) noexcept(noexcept(std::ranges::search(args.str, args.options))) {
#else  // __clang__
    std::function find_first = [](Args args) noexcept(noexcept(std::search(args.str.begin(), args.str.end(),
                                                                           args.options.begin(), args.options.end()))) {
#endif // !__clang__
        auto&& [str, options] = args;
        if (options.empty()) {
            return ""sv;
        }

#ifndef __clang__
        auto result = std::ranges::search(str, options);
        return std::string_view { result.begin(), str.end() };
#else  // __clang__
        auto result = std::search(args.str.begin(), args.str.end(), args.options.begin(), args.options.end());
        return std::string_view { result, str.end() };
#endif // !__clang__
    };
    std::function count_chars = [](std::string_view str) noexcept { return str.size(); };

    auto operation       = mr::compose | std::move(get_string) | std::move(find_first);
    auto count_operation = operation | std::move(count_chars);
    {
        constexpr auto option = "specific"sv;
        auto           result = operation(option);
        assert(result == "specific sub string!"sv);
        assert(count_operation(option) == 20);
    }
    {
        constexpr auto option = "sub"sv;
        auto           result = operation(option);
        assert(result == "sub string!"sv);
        assert(count_operation(option) == 11);
    }
}

namespace utility {
    auto get_func1() {
        return mr::compose | [](std::pair< int, int > x) { return x.first + x.second; } | [](int x) { return x - 1; };
    }
    auto get_func2() {
        return [](int x) { return x + 10; };
    }

    auto get_operation() {
        return mr::compose | get_func1() | get_func2();
    }
} // namespace utility
void test_3() {
    auto my_data = std::vector< std::pair< int, int > > { { 3, 1 }, { 5, 1 }, { 7, 1 }, { 8, 1 } };
    EXPECTED_RESULT(int, 4, 13, 15, 17, 18)

    auto F = utility::get_operation();

    CHECK_RESULT(my_data, F);
}

int main() {
    test_traits();
    test_composable_trait();
    test_function_composition();
    test_lambda_composition();
    test_free_compose();
    test_combination();
    assert(test_function_composition_constexpr() == true);
    assert(test_lambda_composition_constexpr() == true);
    assert(test_callable_struct_constexpr() == true);
    test_construction();
    test_1();
    test_2();
    test_3();
}
