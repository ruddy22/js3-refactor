Feature: Expand and collapse things

  Scenario: Expanding objects
    When I insert "var a = { b: 1, c: 'def' };"
    And I turn on js3-mode
    And I go to the front of the word "b"
    And I press "C-c C-m eo"
    Then I should see:
    """
    var a = {
        b: 1,
        c: 'def'
    };
    """

  Scenario: Expanding objects with comma
    When I insert "var a = { b: 1, c: 'def, ghi' };"
    And I turn on js3-mode
    And I go to the front of the word "b"
    And I press "C-c C-m eo"
    Then I should see:
    """
    var a = {
        b: 1,
        c: 'def, ghi'
    };
    """
