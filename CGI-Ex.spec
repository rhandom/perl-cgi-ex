%define name HTMLForm
%define version @VERSION@
%define release @RELEASE@

%define __find_provides %( echo -n /usr/lib/rpm/find-provides && [ -x /usr/lib/rpm/find-provides.perl ] && echo .perl )
%define __find_requires %( echo -n /usr/lib/rpm/find-requires && [ -x /usr/lib/rpm/find-requires.perl ] && echo .perl )

Summary:        @SUMMARY@
Name:           %{name}
Version:        %{version}
Release:        %{release}
Source0:        http://perlcad.com/download/%{name}-%{version}-%{release}.tar.gz
Group:          Development/Perl
License:        Perl Artistic
Vendor:         Paul Seamons
Packager:       Paul Seamons
BuildRequires:  perl
BuildArch:      noarch
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-buildroot
Provides:       %{name} = %{version}

%description
@DESCRIPTION@

%prep
%setup -q -n %{name}-%{version}-%{release}

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

%clean
rm -rf $RPM_BUILD_ROOT
HERE=`pwd`
cd ..
rm -rf $HERE

%files
%defattr(-,root,root)
%doc README Changes
%{_prefix}

%changelog
* Sat Oct 22 2003 Paul Seamons <>
- first try
