// composition_helper.h
// This header contains a helper implementation to aid in function composition
// while keeping noexcept and const specifiers whenever possible
//
// Created By: Michael Rizkalla
// Date:		02/04/2021

#ifndef COMPOSITION_HELPER_HPP
#define COMPOSITION_HELPER_HPP
// Not using #pragma once since it's not a part of the standard
#include <functional>
#include <stdint.h>
#include <type_traits>
#include <utility>

namespace traits{
      // Helper structs to track function arguments and return type
    template < class... >
    struct list {};
    template < class >
    struct gen_list;
    template < class... >
    struct pop_first_from_list;

    template < template < typename... > class List >
    struct pop_first_from_list< List<> > {
        using type = void;
    };
    template < template < typename... > class List, class FirstType, class... Types >
    struct pop_first_from_list< List< FirstType, Types... > > {
        using type = FirstType;
    };

    template < class... >
    struct count_list_element;
    template < template < typename... > class List >
    struct count_list_element< List<> > {
        static constexpr uint64_t value = 0;
    };
    template < template < typename... > class List, class Type >
    struct count_list_element< List< Type > > {
        static constexpr uint64_t value = 1;
    };
    template < template < typename... > class List, class FirstType, class... Types >
    struct count_list_element< List< FirstType, Types... > > {
        static constexpr uint64_t value = 1 + count_list_element< List< Types... > >::value;
    };

    template < class... >
    struct functor_traits {};

    namespace impl {
        template < class Ret, class... Args >
        struct functor_traits_eval {
            using return_type           = Ret;
            using argument_types        = list< Args... >;
            static constexpr auto value = true;
        };
    } // namespace impl

    template < template < class... > class Func, class Ret, class... Args >
    struct functor_traits< Func< Ret(Args...) > > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = false;
        static constexpr auto has_const    = false;
    };
    template < class Ret, class... Args >
    struct functor_traits< Ret (*)(Args...) > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = false;
        static constexpr auto has_const    = false;
    };
    template < class Ret, class... Args >
    struct functor_traits< Ret (*)(Args...) noexcept > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = true;
        static constexpr auto has_const    = false;
    };
    template < class Closure, class Ret, class... Args >
    struct functor_traits< Ret (Closure::*)(Args...) > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = false;
        static constexpr auto has_const    = false;
    };
    template < class Closure, class Ret, class... Args >
    struct functor_traits< Ret (Closure::*)(Args...) const > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = false;
        static constexpr auto has_const    = true;
    };
    template < class Closure, class Ret, class... Args >
    struct functor_traits< Ret (Closure::*)(Args...) noexcept > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = true;
        static constexpr auto has_const    = false;
    };
    template < class Closure, class Ret, class... Args >
    struct functor_traits< Ret (Closure::*)(Args...) const noexcept > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = true;
        static constexpr auto has_const    = true;
    };
    template < class Type >
    requires(!std::is_class_v< Type >) struct functor_traits< Type > : public impl::functor_traits_eval< Type > {
        using typename impl::functor_traits_eval< Type >::return_type;
        using typename impl::functor_traits_eval< Type >::argument_types;
        using impl::functor_traits_eval< Type >::value;
        static constexpr auto has_noexcept = false;
        static constexpr auto has_const    = false;
    };
    template < class Type >
    requires std::is_class_v< Type > struct functor_traits< Type > : public functor_traits< decltype(&Type::operator()) > {};

    // Type traits
    template < class FirstCallable, class SecondCallable >
    struct is_composable {
      public:
        using first_return                      = typename functor_traits< FirstCallable >::return_type;
        using second_args                       = typename functor_traits< SecondCallable >::argument_types;
        using first_of_second_args              = typename pop_first_from_list< second_args >::type;
        static constexpr auto second_args_count = count_list_element< second_args >::value;

      public:
        static constexpr auto value = std::same_as< std::remove_cvref_t< first_of_second_args >, std::remove_cvref_t< first_return > > && (second_args_count == 1);
    };

    template < class FirstCallable, class SecondCallable >
    concept composable = is_composable< FirstCallable, SecondCallable >::value;
}

namespace fp {
    using namespace traits;
    // Classes
    template < class Signature, class Closure = void >
    struct CompositionFunction;

    namespace impl {

        template < class... >
        struct ClosureToBase;
        template < class... >
        struct CompositionFunctionImpl;
        template < class... >
        struct CompositionFunctionBase;
        template < class Closure, class RetType, class... Args >
        struct NonCompositionFunction {
            NonCompositionFunction() { /*static_assert(false, "The entity is not a callable/functor!");*/
            }
        };

        struct IsNoExcept {};
        struct ICompositionFunction {};

        template < class Closure, class RetType, template < class... > class List, class... Args >
        struct ClosureToBase< Closure, RetType, List< Args... > > {
          public:
            using type = CompositionFunctionBase< Closure, RetType(Args...) >;
        };

        template < class NoExcept, class Closure, class RetType, class... Args >
        using BaseChooser = std::conditional_t<
            std::is_same_v< Closure, void >,
            std::conditional_t< std::is_same_v< NoExcept, IsNoExcept >, CompositionFunctionBase< RetType (*)(Args...) noexcept, RetType(Args...) >,
                                CompositionFunctionBase< RetType (*)(Args...), RetType(Args...) > >,
            std::conditional_t<
                std::conjunction_v< std::is_class< Closure >, std::negation< std::is_base_of< ICompositionFunction, Closure > > >,
                std::conditional_t<
                    functor_traits< Closure >::value,
                    typename ClosureToBase< Closure, typename functor_traits< Closure >::return_type, typename functor_traits< Closure >::argument_types >::type,
                    NonCompositionFunction< Closure, typename functor_traits< Closure >::return_type, typename functor_traits< Closure >::argument_types > >,
                CompositionFunctionBase< Closure, RetType(Args...) > > >;

        template < class FunctionType, class RetType, class... Args >
        struct CompositionFunctionBase< FunctionType, RetType(Args...) > : public ICompositionFunction {

            using UnderlyingFunctionType = std::remove_const_t< FunctionType >;
            using return_type            = RetType;

            constexpr CompositionFunctionBase(UnderlyingFunctionType&& func) : function_(std::move(func)) {}
            constexpr CompositionFunctionBase(const UnderlyingFunctionType& func) : function_(func) {}

            [[nodiscard]] constexpr auto operator()(Args... args) const noexcept(noexcept(std::declval< UnderlyingFunctionType >()(std::declval< Args >()...)))
                -> RetType {
                return function_(args...);
            }

            [[nodiscard]] constexpr auto operator->() const noexcept requires std::is_class_v< std::remove_cvref_t< UnderlyingFunctionType > > { return &function_; }

            template < class SecondCallable, class ComposedRetType = typename functor_traits< std::decay_t< SecondCallable > >::return_type >
            [[nodiscard]] auto Compose(SecondCallable&& secondFunc) const requires composable< std::decay_t< UnderlyingFunctionType >, std::decay_t< SecondCallable > > {
                if constexpr (functor_traits< SecondCallable >::has_const && functor_traits< UnderlyingFunctionType >::has_const) {
                    auto composedFunction = [function__ = function_, secondFunc](Args... args) noexcept(noexcept(std::declval< SecondCallable >()(
                                                std::declval< UnderlyingFunctionType >()(std::declval< Args >()...)))) -> ComposedRetType {
                        return secondFunc(function__(std::forward< Args >(args)...));
                    };
                    return CompositionFunction< ComposedRetType(Args...), decltype(composedFunction) >(std::move(composedFunction));
                } else {
                    auto composedFunction = [function__ = function_, secondFunc](Args... args) mutable noexcept(noexcept(std::declval< SecondCallable >()(
                                                std::declval< UnderlyingFunctionType >()(std::declval< Args >()...)))) -> ComposedRetType {
                        return secondFunc(function__(std::forward< Args >(args)...));
                    };
                    return CompositionFunction< ComposedRetType(Args...), decltype(composedFunction) >(std::move(composedFunction));
                }
            }

          private:
            mutable UnderlyingFunctionType function_;
        };

        template < class Closure, class ClosureType >
        struct CompositionFunctionImpl< Closure, ClosureType > : public BaseChooser< void, ClosureType, void > {
          protected:
            using ImplBase         = BaseChooser< void, ClosureType, void >;
            using ImplFunctionType = typename ImplBase::UnderlyingFunctionType;
            using typename ImplBase::return_type;

          public:
            [[nodiscard]] constexpr CompositionFunctionImpl(ImplFunctionType&& func) : ImplBase(std::move(func)) {}
            [[nodiscard]] constexpr CompositionFunctionImpl(const ImplFunctionType& func) : ImplBase(func) {}
        };

        template < class Closure, class RetType, class... Args >
        struct CompositionFunctionImpl< Closure, RetType(Args...) > : public BaseChooser< void, Closure, RetType, Args... > {
          protected:
            using ImplBase         = BaseChooser< void, Closure, RetType, Args... >;
            using ImplFunctionType = typename ImplBase::UnderlyingFunctionType;
            using typename ImplBase::return_type;

          public:
            [[nodiscard]] constexpr CompositionFunctionImpl(ImplFunctionType&& func) : ImplBase(std::move(func)) {}
            [[nodiscard]] constexpr CompositionFunctionImpl(const ImplFunctionType& func) : ImplBase(func) {}
        };

        template < class Closure, class RetType, class... Args >
        struct CompositionFunctionImpl< Closure, RetType(Args...) noexcept > : public BaseChooser< IsNoExcept, Closure, RetType, Args... > {
          protected:
            using ImplBase         = BaseChooser< IsNoExcept, Closure, RetType, Args... >;
            using ImplFunctionType = typename ImplBase::UnderlyingFunctionType;
            using typename ImplBase::return_type;

          public:
            [[nodiscard]] constexpr CompositionFunctionImpl(ImplFunctionType&& func) : ImplBase(std::move(func)) {}
            [[nodiscard]] constexpr CompositionFunctionImpl(const ImplFunctionType& func) : ImplBase(func) {}
        };

    } // namespace impl

    /// <summary>
    /// Represents a callable
    /// It can be composed with another callable
    /// </summary>
    /// <typeparam name="Signature"></typeparam>
    /// <typeparam name="Closure"></typeparam>
    template < class Signature, class Closure >
    struct CompositionFunction final : public impl::CompositionFunctionImpl< Closure, Signature > {
      private:
        using Base = impl::CompositionFunctionImpl< Closure, Signature >;

      public:
        using FunctionType = typename Base::ImplFunctionType;
        using typename Base::return_type;

        [[nodiscard]] constexpr CompositionFunction() requires(!std::is_class_v< FunctionType >) : Base(nullptr) {}
        [[nodiscard]] constexpr CompositionFunction() requires(std::is_class_v< FunctionType >) : Base(FunctionType {}) {}
        [[nodiscard]] constexpr CompositionFunction(FunctionType&& func) : Base(std::move(func)) {}
        [[nodiscard]] constexpr CompositionFunction(const FunctionType& func) : Base(func) {}
        [[nodiscard]] CompositionFunction(const CompositionFunction&) = default;
        [[nodiscard]] CompositionFunction(CompositionFunction&&)      = default;
        [[nodiscard]] CompositionFunction& operator=(const CompositionFunction&) = default;
        [[nodiscard]] CompositionFunction& operator=(CompositionFunction&&) = default;
        ~CompositionFunction()                                              = default;
    };

    namespace impl {
        template < class FirstCallable, class SecondCallable, class List, class... Args >
        struct Composer;

        template < class FirstCallable, class SecondCallable, class RetType, template < class... > class List, class... Args >
        struct Composer< FirstCallable, SecondCallable, RetType, List< Args... > > {
            [[nodiscard]] constexpr static auto Compose(FirstCallable&& firstFunc, SecondCallable&& secondFunc) {

                if constexpr (functor_traits< SecondCallable >::has_const && functor_traits< FirstCallable >::has_const) {
                    auto composedFunction = [firstFunc_ = firstFunc, secondFunc_ = secondFunc](Args... args) noexcept(
                                                noexcept(std::declval< SecondCallable >()(std::declval< FirstCallable >()(std::declval< Args >()...)))) -> RetType {
                        return secondFunc_(firstFunc_(std::forward< Args >(args)...));
                    };
                    return CompositionFunction< RetType(Args...), decltype(composedFunction) >(std::move(composedFunction));
                } else {
                    auto composedFunction = [firstFunc_ = firstFunc, secondFunc_ = secondFunc](Args... args) mutable noexcept(
                                                noexcept(std::declval< SecondCallable >()(std::declval< FirstCallable >()(std::declval< Args >()...)))) -> RetType {
                        return secondFunc_(firstFunc_(std::forward< Args >(args)...));
                    };
                    return CompositionFunction< RetType(Args...), decltype(composedFunction) >(std::move(composedFunction));
                }
            }
        };

    } // namespace impl

    /// <summary>
    /// Composes two callables into one Compose(X, Y) -> Y(X(Args))
    /// </summary>
    /// <param name="firstFunc">First callable X</param>
    /// <param name="secondFunc">Second callable Y</param>
    /// <returns>CompositionFunction</returns>
    template < class FirstCallable, class SecondCallable, class RetType = typename functor_traits< std::decay_t< SecondCallable > >::return_type,
               class ArgsList = typename functor_traits< std::decay_t< FirstCallable > >::argument_types >
    [[nodiscard]] constexpr auto Compose(FirstCallable  firstFunc,
                                         SecondCallable secondFunc) requires composable< std::decay_t< FirstCallable >, std::decay_t< SecondCallable > > {
        return impl::Composer< FirstCallable, SecondCallable, RetType, ArgsList >::Compose(std::move(firstFunc), std::move(secondFunc));
    }

} // namespace linq

#endif // COMPOSITION_HELPER_HPP
