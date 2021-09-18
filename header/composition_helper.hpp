// composition_helper.h
// This header contains a helper implementation to aid in function composition
// while keeping noexcept and const specifiers whenever possible
//
// Created By:          Michael Rizkalla
// Date:		        02/04/2021
// Updated Rev1.:       18/09/2021

#ifndef COMPOSITION_HELPER_HPP
#define COMPOSITION_HELPER_HPP
// Not using #pragma once since it's not a part of the standard
#include <cstring>
#include <functional>
#include <stdint.h>
#include <tuple>
#include <type_traits>
#include <utility>

namespace traits {
    // Helper structs to track function arguments and return type
    template < class... >
    struct list {};
    template < class... >
    struct pop_first_from_list;
    template < class... >
    struct count_list_element;
    template < class... >
    struct push_element_in_front_of_list;
    namespace impl {
        template < std::size_t Count, class... >
        struct pop_n_element_from_list_impl;

        template < template < typename... > class List, class Type, class... Types >
        struct pop_n_element_from_list_impl< 0, List< Type, Types... > > {
            using type      = Type;
            using list_type = List< Types... >;
        };
        template < std::size_t Count, template < typename... > class List, class Type, class... Types >
        struct pop_n_element_from_list_impl< Count, List< Type, Types... > > {
            using type      = pop_n_element_from_list_impl< Count - 1, List< Types... > >::type;
            using list_type = pop_n_element_from_list_impl< Count - 1, List< Types... > >::list_type;
        };
    } // namespace impl
    template < std::size_t N, class... >
    struct pop_n_element_from_list;

    template < template < typename... > class List >
    struct pop_first_from_list< List<> > {
        using type      = void;
        using list_type = List<>;
    };
    template < template < typename... > class List, class FirstType, class... Types >
    struct pop_first_from_list< List< FirstType, Types... > > {
        using type      = FirstType;
        using list_type = List< Types... >;
    };

    template < std::size_t N, template < typename... > class List, class... Types >
    struct pop_n_element_from_list< N, List< Types... > > {
        static_assert(count_list_element< List< Types... > >::value > N, "List is smaller than N elements");
        using type      = impl::pop_n_element_from_list_impl< N, List< Types... > >::type;
        using list_type = impl::pop_n_element_from_list_impl< N, List< Types... > >::list_type;
    };

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

    template < template < class... > class List, class Element, class... Types >
    struct push_element_in_front_of_list< Element, List< Types... > > {
        using type = List< Element, Types... >;
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
    template < template < class... > class Func, class Ret, class... Args >
    struct functor_traits< Func< Ret(Args...) noexcept > > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = true;
        static constexpr auto has_const    = false;
    };
    template < class Ret, class... Args >
    struct functor_traits< Ret(Args...) > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = false;
        static constexpr auto has_const    = false;
    };
    template < class Ret, class... Args >
    struct functor_traits< Ret(Args...) noexcept > : public impl::functor_traits_eval< Ret, Args... > {
        using typename impl::functor_traits_eval< Ret, Args... >::return_type;
        using typename impl::functor_traits_eval< Ret, Args... >::argument_types;
        using impl::functor_traits_eval< Ret, Args... >::value;
        static constexpr auto has_noexcept = true;
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
    requires std::is_class_v< Type >
    struct functor_traits< Type > : public functor_traits< decltype(&Type::operator()) > {
    };

    // Type trait checks if two callables are composable
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

} // namespace traits

namespace fp {
    using namespace traits;
    // CompisitionFunction Forward Declaration
    template < class... TFunctions >
    struct CompositionFunction;

    namespace impl {

        template < class TFunction, class TFunctionTuple, std::size_t... Indices >
        static constexpr auto make_composition_func(TFunctionTuple functions, TFunction function, std::index_sequence< Indices... >) {
            fp::CompositionFunction new_func(std::get< Indices >(functions)..., function);
            return new_func;
        }

        template < class... TFunctions >
        struct CompositionFunctionTraits {
          private:
            /// <summary>
            /// tests if a type holds a const qualified callable.
            /// Free functions, and function pointers are considered 'const' qualified.
            /// </summary>
            template < class EvalType >
            using ConstTest = std::bool_constant< std::negation_v< std::is_class< std::remove_const_t< EvalType > > > ||
                                                  fp::functor_traits< std::remove_const_t< EvalType > >::has_const >;

            template < class Func >
            static constexpr auto f_has_noexcept() -> std::bool_constant< fp::functor_traits< Func >::has_noexcept >;

            template < class Func, class... Args >
            static constexpr auto f_has_noexcept() -> std::bool_constant< fp::functor_traits< Func >::has_noexcept&& decltype(f_has_noexcept< Args... >())::value >;

            template < class Func >
            static constexpr auto f_has_const() -> ConstTest< Func >;

            template < class Func, class... Args >
            static constexpr auto f_has_const() -> std::bool_constant< ConstTest< Func >::value&& decltype(f_has_const< Args... >())::value >;

          public:
            using return_type      = typename fp::functor_traits< std::tuple_element_t< sizeof...(TFunctions) - 1, std::tuple< TFunctions... > > >::return_type;
            using argument_types   = typename fp::functor_traits< std::tuple_element_t< 0, std::tuple< TFunctions... > > >::argument_types;
            using outer_function_t = std::decay_t< std::tuple_element_t< sizeof...(TFunctions) - 1, std::tuple< TFunctions... > > >;

            template < std::size_t N, class Enable = std::enable_if_t< std::bool_constant< (N < sizeof...(TFunctions)) >::value > >
            using nth_function_t = std::decay_t< std::tuple_element_t< N, std::tuple< TFunctions... > > >;

            static constexpr auto has_noexcept = decltype(f_has_noexcept< TFunctions... >())::value;
            static constexpr auto has_const    = decltype(f_has_const< TFunctions... >())::value;
        };

        template < class TFunctionTuple, class... Args >
        struct FunctionHelper {

            struct first_function_call {};
            struct remaining_function_call {};

          private:
            template < class Type, class TFunction >
            static constexpr auto call(Type arg, TFunction function) {
                return function(std::forward< Type >(arg));
            }

            template < class Type, class TFunction, class... TFunctions >
            static constexpr auto call(Type arg, TFunction function, TFunctions... functions) {
                auto res = function(std::forward< Type >(arg));
                return call(res, functions...);
            }

            template < class TFunction >
            static constexpr auto call_first(TFunction& function, Args... args) {
                return function(std::forward< Args >(args)...);
            }

            template < class TFunction, class... TFunctions >
            static constexpr auto call_first(TFunction& function, TFunctions... functions, Args... args) {
                auto res = function(std::forward< Args >(args)...);
                return call(res, functions...);
            }

            template < std::size_t... Indices >
            static constexpr auto chainCallHelper(TFunctionTuple& functions, std::index_sequence< Indices... >, Args... args) {
                return call_first< decltype(std::get< Indices >(functions))... >(std::get< Indices >(functions)..., std::forward< Args >(args)...);
            }

          public:
            static constexpr auto ChainCalls(TFunctionTuple functions, Args... args) {
                return chainCallHelper(functions, std::make_index_sequence< std::tuple_size< TFunctionTuple >::value > {}, std::forward< Args >(args)...);
            }
        };

        template < class TupleType >
        struct IFunction {
          public:
            constexpr IFunction() = default;

            constexpr virtual TupleType GetFunctions() const noexcept = 0;
        };

        template < bool has_const, class tuple_type, bool has_noexcept, class Ret, class... Args >
        struct FunctionBase_;

        template < class TupleType, bool has_noexcept, class Ret, template < class... > class List, class... Args >
        struct FunctionBase_< true, TupleType, has_noexcept, Ret, List< Args... > > : public IFunction< TupleType > {

          protected:
            using IBase = IFunction< TupleType >;

          public:
            constexpr FunctionBase_(TupleType&& functions) : mFunctions(std::move(functions)) {
            }

            constexpr Ret operator()(Args... args) const noexcept(has_noexcept) {
                return FunctionHelper< TupleType, Args... >::ChainCalls(mFunctions, std::forward< Args >(args)...);
            }

            constexpr TupleType GetFunctions() const noexcept override {
                return mFunctions;
            };

          protected:
            TupleType mFunctions;
        };

        template < class TupleType, bool has_noexcept, class Ret, template < class... > class List, class... Args >
        struct FunctionBase_< false, TupleType, has_noexcept, Ret, List< Args... > > : public IFunction< TupleType > {

          private:
            using IBase = IFunction< TupleType >;

          public:
            FunctionBase_(TupleType&& functions) : mFunctions(std::move(functions)) {
            }

            Ret operator()(Args... args) noexcept(has_noexcept) {
                return FunctionHelper< TupleType, Args... >::ChainCalls(mFunctions, std::forward< Args >(args)...);
            }

            constexpr TupleType GetFunctions() const noexcept override {
                return mFunctions;
            };

          protected:
            TupleType mFunctions;
        };

        template < class... TFunctions >
        using FunctionBase = FunctionBase_< CompositionFunctionTraits< TFunctions... >::has_const, std::tuple< std::decay_t< TFunctions >... >,
                                            CompositionFunctionTraits< TFunctions... >::has_noexcept, typename CompositionFunctionTraits< TFunctions... >::return_type,
                                            typename CompositionFunctionTraits< TFunctions... >::argument_types >;

    } // namespace impl

    template < class... TFunctions >
    struct CompositionFunction : public impl::FunctionBase< TFunctions... > {
      public:
        using return_type = typename impl::CompositionFunctionTraits< TFunctions... >::return_type;
        using arg_list    = typename impl::CompositionFunctionTraits< TFunctions... >::argument_types;
        using tuple_type  = std::tuple< std::decay_t< TFunctions >... >;

#ifndef COMPOSITION_HELPER_DEBUG
      private:
#endif // COMPOSITION_HELPER_DEBUG

        static constexpr auto no_except_property = impl::CompositionFunctionTraits< TFunctions... >::has_noexcept;
        static constexpr auto const_property     = impl::CompositionFunctionTraits< TFunctions... >::has_const;

        using Base = impl::FunctionBase< TFunctions... >;

      public:
        constexpr CompositionFunction(TFunctions... functions) : Base(std::forward_as_tuple< std::decay_t< TFunctions >... >(std::forward< TFunctions >(functions)...)) {
        }

        template < class TFunction, class Enable = std::enable_if_t<
                                        is_composable< typename impl::CompositionFunctionTraits< TFunctions... >::outer_function_t, std::decay_t< TFunction > >::value > >
        constexpr auto Compose(TFunction function) const {
            return impl::make_composition_func< TFunction >(Base::mFunctions, std::forward< TFunction >(function),
                                                            std::make_index_sequence< std::tuple_size< tuple_type >::value > {});
        }
    };

    namespace impl {
        template < class FirstCallable, class SecondCallable, class... Args >
        struct Composer;

        template < class SecondCallable, class... Args >
        struct Composer< CompositionFunction< Args... >, SecondCallable > {
            [[nodiscard]] constexpr static auto Compose(CompositionFunction< Args... > firstFunc, SecondCallable secondFunc) {
                return impl::make_composition_func< SecondCallable >(
                    firstFunc.GetFunctions(), std::forward< SecondCallable >(secondFunc),
                    std::make_index_sequence< std::tuple_size< typename CompositionFunction< Args... >::tuple_type >::value > {});
            }
        };

        template < class FirstCallable, class... Args >
        struct Composer< FirstCallable, CompositionFunction< Args... > > {
            [[nodiscard]] constexpr static auto Compose(FirstCallable firstFunc, CompositionFunction< Args... > secondFunc) {
                return impl::make_composition_func< FirstCallable >(
                    secondFunc.GetFunctions(), std::forward< FirstCallable >(firstFunc),
                    std::make_index_sequence< std::tuple_size< typename CompositionFunction< Args... >::tuple_type >::value > {});
            }
        };

        template < class FirstCallable, class SecondCallable >
        struct Composer< FirstCallable, SecondCallable > {
            [[nodiscard]] constexpr static auto Compose(FirstCallable firstFunc, SecondCallable secondFunc) {
                return fp::CompositionFunction { std::forward< FirstCallable >(firstFunc), std::forward< SecondCallable >(secondFunc) };
            }
        };

    } // namespace impl

    /// <summary>
    /// Composes two callables into one Compose(X, Y) -> Y(X(Args))
    /// </summary>
    template < class FirstCallable, class SecondCallable,
               class Enable = std::enable_if_t< is_composable< std::decay_t< FirstCallable >, std::decay_t< SecondCallable > >::value > >
    [[nodiscard]] constexpr auto Compose(FirstCallable firstFunc, SecondCallable secondFunc) {
        return impl::Composer< FirstCallable, SecondCallable >::Compose(std::forward< FirstCallable >(firstFunc), std::forward< SecondCallable >(secondFunc));
    }

} // namespace fp

#endif // COMPOSITION_HELPER_HPP
