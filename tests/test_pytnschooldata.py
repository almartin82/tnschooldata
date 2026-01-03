"""
Tests for pytnschooldata Python wrapper.

Minimal smoke tests - the actual data logic is tested by R testthat.
These just verify the Python wrapper imports and exposes expected functions.
"""

import pytest


def test_import_package():
    """Package imports successfully."""
    import pytnschooldata
    assert pytnschooldata is not None


def test_has_fetch_enr():
    """fetch_enr function is available."""
    import pytnschooldata
    assert hasattr(pytnschooldata, 'fetch_enr')
    assert callable(pytnschooldata.fetch_enr)


def test_has_get_available_years():
    """get_available_years function is available."""
    import pytnschooldata
    assert hasattr(pytnschooldata, 'get_available_years')
    assert callable(pytnschooldata.get_available_years)


def test_has_version():
    """Package has a version string."""
    import pytnschooldata
    assert hasattr(pytnschooldata, '__version__')
    assert isinstance(pytnschooldata.__version__, str)
