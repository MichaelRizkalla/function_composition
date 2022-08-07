// composition_helper.h
// This header contains a helper implementation to aid in function composition
// while keeping noexcept and const specifiers whenever possible
//
// Created By: Michael Rizkalla
// Date:		02/04/2021

#ifndef COMPOSITION_HELPER_HPP
#define COMPOSITION_HELPER_HPP
// Not using #pragma once since it's not a part of the standard
#include <cstdint>
#include <functional>
#include <type_traits>
#include <utility>

namespace mr {

    namespace mr_traits {
        template < class... >
        struct count_list_element;
        template < template < typename... > class List >
        struct count_list_element< List<> > {
            static constexpr std::uint64_t value = 0;
        };
        template < template < typename... > class List, class... Types >
        struct count_list_element< List< Types... > > {
            static constexpr std::uint64_t value = sizeof...(Types);
        };

        template < std::size_t Idx, class... >
        struct get_nth_type;
        template <>
        struct get_nth_type< 0, std::tuple<> > {
            using type = void;
        };
        template < class FirstType, class... Types >
        struct get_nth_type< 0, std::tuple< FirstType, Types... > > {
            using type = FirstType;
        };
        template < std::size_t Idx, class FirstType, class... Types >
        struct get_nth_type< Idx, std::tuple< FirstType, Types... > > {
            using type = typename get_nth_type< Idx - 1, std::tuple< Types... > >::type;
        };
        template < std::size_t Idx, class T >
        using get_nth_type_t = typename get_nth_type< Idx, T >::type;
        template < class T >
        using get_first_type_t = typename get_nth_type< 0, T >::type;

        namespace impl {
            template < class T >
            void Construct_array(T(&&)[1]);
        }
        template < class From, class To >
        concept is_sanely_convertible = requires {
            impl::Construct_array< To >({ std::declval< From >() });
        };

        template < class... >
        struct callable_traits {};

        namespace impl {
            template < class Ret, bool IsConst, bool IsNoexcept, class... Args >
            struct callable_traits_eval {
                using return_type                 = Ret;
                using argument_types              = std::tuple< Args... >;
                using first_argument              = std::conditional_t< ((std::tuple_size_v< argument_types >) > 0),
                                                           get_first_type_t< argument_types >, void >;
                static constexpr auto is_noexcept = IsNoexcept;
                static constexpr auto is_const    = IsConst;
            };
        } // namespace impl

#define CALLABLE_TRAITS_DEF(FUNCTYPE, RETTYPE, ARGTYPE, FUNCSTYLE, NOEXCEPT_BOOL, CONST_BOOL) \
    template < FUNCTYPE, class RETTYPE, class... ARGTYPE >                                    \
    struct callable_traits< FUNCSTYLE >                                                       \
        : public impl::callable_traits_eval< RETTYPE, CONST_BOOL, NOEXCEPT_BOOL, ARGTYPE... > {};

        CALLABLE_TRAITS_DEF(template < class... > class Func, Ret, Args, Func< Ret(Args...) >, false, false)
        CALLABLE_TRAITS_DEF(template < class... > class Func, Ret, Args, Func< Ret(Args...) const >, false, true)
        CALLABLE_TRAITS_DEF(template < class... > class Func, Ret, Args, Func< Ret(Args...) noexcept >, true, false)
        CALLABLE_TRAITS_DEF(template < class... > class Func, Ret, Args, Func< Ret(Args...) const noexcept >, true,
                            true)
        CALLABLE_TRAITS_DEF(class Closure, Ret, Args, Ret (Closure::*)(Args...), false, false)
        CALLABLE_TRAITS_DEF(class Closure, Ret, Args, Ret (Closure::*)(Args...) const, false, true)
        CALLABLE_TRAITS_DEF(class Closure, Ret, Args, Ret (Closure::*)(Args...) noexcept, true, false)
        CALLABLE_TRAITS_DEF(class Closure, Ret, Args, Ret (Closure::*)(Args...) const noexcept, true, true)
#undef CALLABLE_TRAITS_DEF

#define CALLABLE_TRAITS_DEF(RETTYPE, ARGTYPE, FUNCSTYLE, NOEXCEPT_BOOL, CONST_BOOL)             \
    template < class RETTYPE, class... ARGTYPE >                                                \
    struct callable_traits< FUNCSTYLE >                                                         \
        : public impl::callable_traits_eval< RETTYPE, CONST_BOOL, NOEXCEPT_BOOL, ARGTYPE... > { \
        using type = RETTYPE (*)(ARGTYPE...) noexcept(NOEXCEPT_BOOL);                           \
    };

        CALLABLE_TRAITS_DEF(Ret, Args, Ret (*)(Args...), false, false)
        CALLABLE_TRAITS_DEF(Ret, Args, Ret (*)(Args...) noexcept, true, false)
        CALLABLE_TRAITS_DEF(Ret, Args, Ret (&)(Args...), false, false)
        CALLABLE_TRAITS_DEF(Ret, Args, Ret (&)(Args...) noexcept, true, false)
        CALLABLE_TRAITS_DEF(Ret, Args, Ret(Args...), false, false)
        CALLABLE_TRAITS_DEF(Ret, Args, Ret(Args...) noexcept, true, false)
#undef CALLABLE_TRAITS_DEF

        template < class Type >
        struct callable_traits< Type > : public callable_traits< decltype(&Type::operator()) > {};

        // concept checks if two callables are composable
        namespace impl {
            template < class T >
            constexpr T get_type() noexcept;

            template < class FirstCallable, class SecondCallable, std::size_t... Idx >
            concept composable_impl = requires(FirstCallable c1, SecondCallable c2, std::index_sequence< Idx... > idx) {
                c2(c1(
                    get_type< get_nth_type_t< Idx, typename callable_traits< FirstCallable >::argument_types > >()...));
            };
            template < class FirstCallable, class SecondCallable, std::size_t... Idx >
            concept not_composable = !composable_impl< FirstCallable, SecondCallable, Idx... >;

            template < class FirstCallable, class SecondCallable, std::size_t... Idx >
            constexpr std::true_type CheckComposable(FirstCallable&&, SecondCallable&&,
                                                     std::index_sequence< Idx... >) requires
                composable_impl< FirstCallable, SecondCallable, Idx... > {
                return std::true_type {};
            }
            template < class FirstCallable, class SecondCallable, std::size_t... Idx >
            constexpr std::false_type CheckComposable(FirstCallable&&, SecondCallable&&,
                                                      std::index_sequence< Idx... >) requires
                not_composable< FirstCallable, SecondCallable, Idx... > {
                return std::false_type {};
            }

            template < class FirstCallable, class SecondCallable,
                       class List = typename callable_traits< FirstCallable >::argument_types >
            constexpr auto CheckComposable_(FirstCallable c1, SecondCallable c2) {
                if constexpr ((std::tuple_size_v< List >) > 0) {
                    return CheckComposable(std::forward< std::decay_t< FirstCallable > >(c1),
                                           std::forward< std::decay_t< SecondCallable > >(c2),
                                           std::make_index_sequence< std::tuple_size_v< List > > {});
                } else {
                    constexpr auto value =
                        composable_impl< std::decay_t< FirstCallable >, std::decay_t< SecondCallable > >;
                    return std::bool_constant< value > {};
                }
            }
            template < class... >
            constexpr auto CheckComposable_(...) -> std::false_type;

            template < class FirstCallable, class SecondCallable >
            concept composable_helper = requires(std::decay_t< FirstCallable > c1, std::decay_t< SecondCallable > c2) {
                {
                    impl::CheckComposable_(std::forward< std::decay_t< FirstCallable > >(c1),
                                           std::forward< std::decay_t< SecondCallable > >(c2))
                    } -> std::same_as< std::true_type >;
            };
        } // namespace impl

        template < class FirstCallable, class SecondCallable >
        concept composable = impl::composable_helper< FirstCallable, SecondCallable > &&
                             is_sanely_convertible < typename callable_traits< FirstCallable >::return_type,
        typename callable_traits< SecondCallable >::first_argument > ;

    } // namespace mr_traits

    namespace details {
        template < class CPO, class Fn, class... Args >
        struct composing_type;

        template < class... >
        struct apply_composition {
            template < class L, class R >
            constexpr decltype(auto) operator()(L, R) const noexcept;
        };

        template < class Callable >
        struct [[nodiscard]] Storage {
          private:
            static constexpr auto is_const = mr_traits::callable_traits< Callable >::is_const;
            using return_type              = std::conditional_t< is_const, const Callable&, Callable& >;

          public:
            template < class T >
            requires(std::is_constructible_v< Callable, T >) constexpr Storage(T&& t) noexcept(
                noexcept(std::is_nothrow_constructible_v< Callable, T >)) :
                callable(std::forward< T >(t)) {
            }
            constexpr Storage(const Storage&)     = default;
            constexpr Storage(Storage&&) noexcept = default;
            constexpr Storage& operator=(const Storage&) = default;
            constexpr Storage& operator=(Storage&&) noexcept = default;
            constexpr ~Storage()                             = default;

            constexpr decltype(auto) Get() const& noexcept {
                return const_cast< return_type >(callable);
            }
            constexpr decltype(auto) Get() & noexcept {
                return callable;
            }
            constexpr decltype(auto) Get() const&& noexcept {
                return std::move(callable);
            }
            constexpr decltype(auto) Get() && noexcept {
                return std::move(callable);
            }

            Callable callable;
        };

        namespace pipeline {
            template < class L, class R >
            concept is_reconstructible_without_cvref = std::constructible_from< std::remove_cvref_t< L >, L > &&
                std::constructible_from< std::remove_cvref_t< R >, R >;

            template < class, class, class... >
            struct composable_pipeline;
        } // namespace pipeline

        template < class Derived, class... Args >
        struct [[nodiscard]] composable_type {
          public:
            // TODO check these 4
            template < class T >
            requires(mr_traits::composable< Derived, T >&&
                         pipeline::is_reconstructible_without_cvref< const Derived&, const T& >)
                [[nodiscard]] constexpr auto
                operator|(const composable_type< T >& rhs) const& noexcept(
                    noexcept(pipeline::composable_pipeline< const Derived&, const T&, Args... > {
                        static_cast< const Derived& >(*this), static_cast< const T& >(rhs) })) {
                return pipeline::composable_pipeline< const Derived&, const T&, Args... > {
                    static_cast< const Derived& >(*this), static_cast< const T& >(rhs)
                };
            }
            template < class T >
            requires(
                mr_traits::composable< Derived, T >&& pipeline::is_reconstructible_without_cvref< const Derived&, T >)
                [[nodiscard]] constexpr auto
                operator|(composable_type< T >&& rhs) const& noexcept(
                    noexcept(pipeline::composable_pipeline< const Derived&, T&&, Args... > {
                        static_cast< const Derived& >(*this), static_cast< T&& >(rhs) })) {
                return pipeline::composable_pipeline< const Derived&, T&&, Args... > {
                    static_cast< const Derived& >(*this), static_cast< T&& >(rhs)
                };
            }
            template < class T >
            requires(
                mr_traits::composable< Derived, T >&& pipeline::is_reconstructible_without_cvref< Derived&, const T& >)
                [[nodiscard]] constexpr auto
                operator|(const composable_type< T >& rhs) & noexcept(
                    noexcept(pipeline::composable_pipeline< Derived&, const T&, Args... > {
                        static_cast< Derived& >(*this), static_cast< const T& >(rhs) })) {
                return pipeline::composable_pipeline< Derived&, const T&, Args... > { static_cast< Derived& >(*this),
                                                                                      static_cast< const T& >(rhs) };
            }
            template < class T >
            requires(mr_traits::composable< Derived, T >&& pipeline::is_reconstructible_without_cvref< Derived&, T >)
                [[nodiscard]] constexpr auto
                operator|(composable_type< T >&& rhs) & noexcept(
                    noexcept(pipeline::composable_pipeline< Derived&, T&&, Args... > { static_cast< Derived& >(*this),
                                                                                       static_cast< T&& >(rhs) })) {
                return pipeline::composable_pipeline< Derived&, T&&, Args... > { static_cast< Derived& >(*this),
                                                                                 static_cast< T&& >(rhs) };
            }
            template < class T >
            requires(mr_traits::composable< Derived, T >&&
                         pipeline::is_reconstructible_without_cvref< const Derived, const T& >)
                [[nodiscard]] constexpr auto
                operator|(const composable_type< T >& rhs) const&& noexcept(
                    noexcept(pipeline::composable_pipeline< const Derived&&, const T&, Args... > {
                        static_cast< const Derived&& >(*this), static_cast< const T& >(rhs) })) {
                return pipeline::composable_pipeline< const Derived&&, const T&, Args... > {
                    static_cast< const Derived&& >(*this), static_cast< const T& >(rhs)
                };
            }
            template < class T >
            requires(
                mr_traits::composable< Derived, T >&& pipeline::is_reconstructible_without_cvref< const Derived, T >)
                [[nodiscard]] constexpr auto
                operator|(composable_type< T >&& rhs) const&& noexcept(
                    noexcept(pipeline::composable_pipeline< const Derived&&, T&&, Args... > {
                        static_cast< const Derived&& >(*this), static_cast< T&& >(rhs) })) {
                return pipeline::composable_pipeline< const Derived&&, T&&, Args... > {
                    static_cast< const Derived&& >(*this), static_cast< T&& >(rhs)
                };
            }
            template < class T >
            requires(
                mr_traits::composable< Derived, T >&& pipeline::is_reconstructible_without_cvref< Derived, const T& >)
                [[nodiscard]] constexpr auto
                operator|(const composable_type< T >& rhs) && noexcept(
                    noexcept(pipeline::composable_pipeline< Derived&&, const T&, Args... > {
                        static_cast< Derived&& >(*this), static_cast< const T& >(rhs) })) {
                return pipeline::composable_pipeline< Derived&&, const T&, Args... > { static_cast< Derived&& >(*this),
                                                                                       static_cast< const T& >(rhs) };
            }
            template < class T >
            requires(mr_traits::composable< Derived, T >&& pipeline::is_reconstructible_without_cvref< Derived, T >)
                [[nodiscard]] constexpr auto
                operator|(composable_type< T >&& rhs) && noexcept(
                    noexcept(pipeline::composable_pipeline< Derived&&, T&&, Args... > { static_cast< Derived&& >(*this),
                                                                                        static_cast< T&& >(rhs) })) {
                return pipeline::composable_pipeline< Derived&&, T&&, Args... > { static_cast< Derived&& >(*this),
                                                                                  static_cast< T&& >(rhs) };
            }

            template < class T >
            [[nodiscard]] constexpr auto operator|(T&& rhs) const& noexcept(
                noexcept(apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< const Derived& >(*this),
                                                                                std::forward< T >(rhs)))) {
                return apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< const Derived& >(*this),
                                                                              std::forward< T >(rhs));
            }
            template < class T >
            [[nodiscard]] constexpr auto operator|(T&& rhs) & noexcept(
                noexcept(apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< Derived& >(*this),
                                                                                std::forward< T >(rhs)))) {
                return apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< Derived& >(*this),
                                                                              std::forward< T >(rhs));
            }
            template < class T >
            [[nodiscard]] constexpr auto operator|(T&& rhs) const&& noexcept(
                noexcept(apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< const Derived&& >(*this),
                                                                                std::forward< T >(rhs)))) {
                return apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< const Derived&& >(*this),
                                                                              std::forward< T >(rhs));
            }
            template < class T >
            [[nodiscard]] constexpr auto operator|(T&& rhs) && noexcept(
                noexcept(apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< Derived&& >(*this),
                                                                                std::forward< T >(rhs)))) {
                return apply_composition< std::remove_cvref_t< Derived > > {}(static_cast< Derived&& >(*this),
                                                                              std::forward< T >(rhs));
            }
        };

        namespace pipeline {
            template < class First, class Second, class... Args >
            struct composable_pipeline
                : public composable_type< composable_pipeline< First, Second, Args... >, Args... > {
              private:
                using Base = composable_type< composable_pipeline< First, Second, Args... >, Args... >;

              public:
                static_assert(mr_traits::composable< std::decay_t< First >, std::decay_t< Second > >,
                              "First callable and second callable must be composable as second(first(Args...))!");
                template < class T1, class T2 >
                constexpr explicit composable_pipeline(T1&& v1, T2&& v2) noexcept(
                    std::is_nothrow_convertible_v< T1, std::decay_t< First > >&&
                        std::is_nothrow_convertible_v< T2, std::decay_t< Second > >) :
                    first(std::forward< T1 >(v1)), second(std::forward< T2 >(v2)) {
                }

                [[nodiscard]] constexpr auto operator()(Args... args) const
                    noexcept(noexcept(std::invoke(second.Get(),
                                                  std::invoke(first.Get(), std::forward< Args >(args)...)))) {
                    return std::invoke(second.Get(), std::invoke(first.Get(), std::forward< Args >(args)...));
                }

                template < class T >
                constexpr auto Compose(T&& func) const& noexcept(noexcept((*this).operator|(std::forward< T >(func)))) {
                    return (*this).operator|(std::forward< T >(func));
                }
                template < class T >
                constexpr auto Compose(T&& func) & noexcept(noexcept((*this).operator|(std::forward< T >(func)))) {
                    return (*this).operator|(std::forward< T >(func));
                }
                template < class T >
                constexpr auto
                    Compose(T&& func) const&& noexcept(noexcept((*this).operator|(std::forward< T >(func)))) {
                    return std::move(*this).operator|(std::forward< T >(func));
                }
                template < class T >
                constexpr auto Compose(T&& func) && noexcept(noexcept((*this).operator|(std::forward< T >(func)))) {
                    return std::move(*this).operator|(std::forward< T >(func));
                }

              private:
                Storage< std::decay_t< First > >  first;
                Storage< std::decay_t< Second > > second;
            };

            template < class First, class Second, class... Args >
            composable_pipeline(First, Second, Args...) -> composable_pipeline< First, Second, Args... >;
        } // namespace pipeline

        template < class CPO, class Fn, class... Args >
        struct [[nodiscard]] composing_type : public composable_type< composing_type< CPO, Fn, Args... >, Args... > {
          public:
            template < class T >
            constexpr explicit composing_type(T&& fn) noexcept(noexcept(std::is_nothrow_constructible_v< Fn, T >)) :
                callable(std::forward< T >(fn)) {
            }

            [[nodiscard]] constexpr auto operator()(Args... args) const
                noexcept(noexcept(std::invoke(callable.Get(), std::forward< Args >(args)...))) {
                return std::invoke(callable.Get(), std::forward< Args >(args)...);
            }

            template < class Second >
            requires(std::invocable< CPO, Fn, Second >) [[nodiscard]] constexpr decltype(auto)
                Compose(Second&& second) & noexcept(
                    noexcept(composing_type::ComposeInternal(*this, std::forward< Second >(second)))) {
                return composing_type::ComposeInternal(*this, std::forward< Second >(second));
            }
            template < class Second >
            requires(std::invocable< CPO, Fn, Second >) [[nodiscard]] constexpr decltype(auto) Compose(Second&& second)
                const& noexcept(noexcept(composing_type::ComposeInternal(*this, std::forward< Second >(second)))) {
                return composing_type::ComposeInternal(*this, std::forward< Second >(second));
            }

            template < class Second >
            requires(std::invocable< CPO, Fn, Second >) [[nodiscard]] constexpr decltype(auto)
                Compose(Second&& second) && noexcept(
                    noexcept(composing_type::ComposeInternal(std::move(*this), std::forward< Second >(second)))) {
                return composing_type::ComposeInternal(std::move(*this), std::forward< Second >(second));
            }
            template < class Second >
            requires(std::invocable< CPO, Fn, Second >) [[nodiscard]] constexpr decltype(auto)
                Compose(Second&& second) const&& noexcept(
                    noexcept(composing_type::ComposeInternal(std::move(*this), std::forward< Second >(second)))) {
                return composing_type::ComposeInternal(std::move(*this), std::forward< Second >(second));
            }

          private:
            template < class Myself, class Second >
            [[nodiscard]] static constexpr decltype(auto) ComposeInternal(Myself&& self, Second second) noexcept(
                noexcept(CPO {}(std::forward< Myself >(self).Get(), std::forward< Second >(second)))) {
                return CPO {}(std::forward< Myself >(self).Get(), std::forward< Second >(second));
            }

            constexpr decltype(auto) Get() const& noexcept {
                return callable.Get();
            }
            constexpr decltype(auto) Get() & noexcept {
                return callable.Get();
            }
            constexpr decltype(auto) Get() const&& noexcept {
                return std::move(callable).Get();
            }
            constexpr decltype(auto) Get() && noexcept {
                return std::move(callable).Get();
            }

            Storage< Fn > callable;
        };

        template < class First, class Second, class... Args >
        struct apply_composition< pipeline::composable_pipeline< First, Second, Args... > > {
            template < class L, class R >
            [[nodiscard]] constexpr decltype(auto) operator()(L&& lhs, R&& rhs) const
                noexcept(noexcept(pipeline::composable_pipeline< L, R, Args... >(std::forward< L >(lhs),
                                                                                 std::forward< R >(rhs)))) {
                return pipeline::composable_pipeline< L, R, Args... >(std::forward< L >(lhs), std::forward< R >(rhs));
            }
        };

        template < class CPO, class Fn, class... Args >
        struct apply_composition< composing_type< CPO, Fn, Args... > > {
            template < class L, class R >
            [[nodiscard]] constexpr decltype(auto) operator()(L&& lhs, R&& rhs) const
                noexcept(noexcept(std::forward< L >(lhs).Compose(std::forward< R >(rhs)))) {
                return std::forward< L >(lhs).Compose(std::forward< R >(rhs));
            }
        };

        namespace impl {
            template < class CPO, class Callable, class... Args >
            struct propagate_to_composing_type;
            template < class First, class Second, class... Args >
            struct propagate_to_pipeline_type;

            template < class CPO, class Callable, class... Args >
            struct propagate_to_composing_type< CPO, Callable, std::tuple< Args... > > {
                using type = composing_type< CPO, Callable, Args... >;
            };
            template < class First, class Second, class... Args >
            struct propagate_to_pipeline_type< First, Second, std::tuple< Args... > > {
                using type = pipeline::composable_pipeline< First, Second, Args... >;
            };
        } // namespace impl

        template < class CPO, class Callable >
        struct get_composing_type {
            using type = typename impl::propagate_to_composing_type<
                CPO, Callable, typename mr_traits::callable_traits< Callable >::argument_types >::type;
        };
        template < class First, class Second >
        struct get_pipeline {
            using pipeline = typename impl::propagate_to_pipeline_type<
                First, Second, typename mr_traits::callable_traits< First >::argument_types >::type;
        };

    } // namespace details

    template < class First, class Second >
    class composition_function;

    namespace details::Compose_Fn {
        struct Do_compose {
            template < class First, class Second >
            [[nodiscard]] constexpr auto operator()(First&& f, Second&& s) const {
                return composition_function< std::decay_t< First >, std::decay_t< Second > >(std::forward< First >(f),
                                                                                             std::forward< Second >(s));
            }

            template < class Fn >
            [[nodiscard]] constexpr auto operator()(Fn&& fn) const
                noexcept(noexcept(std::is_nothrow_constructible_v< std::decay_t< Fn >, Fn >)) {
                return typename details::get_composing_type< Do_compose, std::decay_t< Fn > >::type(
                    std::forward< Fn >(fn));
            }

            template < class Fn >
            [[nodiscard]] constexpr auto operator|(Fn&& fn) const
                noexcept(noexcept(this->operator()(std::forward< Fn >(fn)))) {
                return this->operator()(std::forward< Fn >(fn));
            }
        };
    } // namespace details::Compose_Fn
    inline constexpr details::Compose_Fn::Do_compose compose;

    template < class First, class Second >
    class [[nodiscard]] composition_function : public details::get_pipeline< First, Second >::pipeline {
      private:
        using Base = typename details::get_pipeline< First, Second >::pipeline;

      public:
        template < class T1, class T2 >
        constexpr explicit composition_function(T1&& v1, T2&& v2) noexcept(noexcept(Base(std::forward< T1 >(v1),
                                                                                         std::forward< T2 >(v2)))) :
            Base(std::forward< T1 >(v1), std::forward< T2 >(v2)) {
        }
    };

    template < class First >
    class [[nodiscard]] composition_function< First, void >
        : public details::get_composing_type< decltype(compose), First >::type {
      private:
        using Base = typename details::get_composing_type< decltype(compose), First >::type;

      public:
        template < class F >
        constexpr explicit composition_function(F&& func) noexcept(noexcept(Base(std::forward< F >(func)))) :
            Base(std::forward< F >(func)) {
        }
    };

    template < class First, class Second >
    composition_function(First, Second) -> composition_function< First, Second >;
    template < class First >
    composition_function(First) -> composition_function< First, void >;

} // namespace mr

#endif // COMPOSITION_HELPER_HPP
