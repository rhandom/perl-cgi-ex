%define name CGI-Ex
%define version 2.03

%define __find_provides %( echo -n /usr/lib/rpm/find-provides && [ -x /usr/lib/rpm/find-provides.perl ] && echo .perl )
%define __find_requires %( echo -n /usr/lib/rpm/find-requires && [ -x /usr/lib/rpm/find-requires.perl ] && echo .perl )

Summary:        @SUMMARY@
Name:           %{name}
Version:        %{version}
Release:        1
Source0:        http://seamons.com/cgi_ex/%{name}-%{version}.tar.gz
Group:          Development/Perl
License:        Perl Artistic
Vendor:         Paul Seamons
Packager:       Paul Seamons
BuildRequires:  perl
BuildArch:      noarch
BuildRoot:      %{_tmppath}/%{name}-%{version}-buildroot
Provides:       %{name} = %{version}

%description
CGI::Ex provides a suite of utilities to make writing CGI scripts
more enjoyable.  Although they can all be used separately, the
main functionality of each of the modules is best represented in
the CGI::Ex::App module.  CGI::Ex::App takes CGI application building
to the next step.  CGI::Ex::App is not quite a framework (which normally
includes prebuilt html) instead CGI::Ex::App is an extended application
flow that dramatically reduces CGI build time in most cases.  It does so
using as little magic as possible.  See L<CGI::Ex::App>.

The main functionality is provided by several other modules that
may be used separately, or together through the CGI::Ex interface.

%prep
%setup -q -n %{name}-%{version}

%build
%{__perl} Makefile.PL
%{__make} OPTIMIZE="$RPM_OPT_FLAGS"

%install
rm -rf $RPM_BUILD_ROOT

# do the build
%{makeinstall} PREFIX=$RPM_BUILD_ROOT%{_prefix}
#if [ -x /usr/lib/rpm/brp-mandrake ] ; then
#  /usr/lib/rpm/brp-mandrake
#elif [ -x /usr/lib/brp-compress ] ; then
#  /usr/lib/rpm/brp-compress
#fi

# Clean up some files we don't want/need
find $RPM_BUILD_ROOT%{_prefix} -type d | tac | xargs rmdir --ign
find $RPM_BUILD_ROOT%{_prefix} | grep i386 | tac | xargs rm -rf

%clean
rm -rf $RPM_BUILD_ROOT
HERE=`pwd`
cd ..
rm -rf $HERE

%files
%defattr(-,root,root)
#%doc README Changes
%{_prefix}

%changelog
* Sat Nov 11 2003 Paul Seamons <>
- first try
