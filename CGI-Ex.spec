%define name CGI-Ex
%define version 0.93

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
CGI::Ex is a Perl module that offers an extended suite of
functionality, over and above that offered by CGI, HTML::FillInForm,
and the host of Validator scripts on CPAN.  CGI::Ex tries to use the
best functions from existing modules and extend them with rich
functionality.

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
