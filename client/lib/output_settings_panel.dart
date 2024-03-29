import 'dart:async';

import 'package:built_collection/built_collection.dart';
import 'package:client/packed_button_row.dart';
import 'package:core/pubsub.dart';
import 'package:core/streams.dart';
import 'package:flutter/cupertino.dart';

import 'package:core/deck.dart';
import 'package:flutter_utils/widget_modifiers.dart';

import 'colours.dart';
import 'settings_controls.dart';

class OutputSettingsPanel extends StatefulWidget {
  final PubSub<Update> updateStream;

  const OutputSettingsPanel({
    super.key,
    required this.updateStream,
  });

  @override
  createState() => _OutputSettingsPanelState();
}

class _OutputSettingsPanelState extends State<OutputSettingsPanel> {
  var programme = Programme.new_();
  var selected = '';

  late StreamSubscription<Update> _updateStreamSubscription;

  void processUpdate(Update update) =>
      setState(() => programme = update.programme);

  @override
  void initState() {
    super.initState();

    _updateStreamSubscription = widget.updateStream.subscribe(processUpdate);
  }

  @override
  void didUpdateWidget(covariant OutputSettingsPanel oldWidget) {
    super.didUpdateWidget(oldWidget);

    if (widget.updateStream != oldWidget.updateStream) {
      _updateStreamSubscription.cancel();
      _updateStreamSubscription = widget.updateStream.subscribe(processUpdate);
    }
  }

  @override
  dispose() {
    _updateStreamSubscription.cancel();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return Column(
      children: [
        Row(
          children: [
            Text("Settings", style: ColourPalette.of(context).headingStyle),
            const Spacer(),
            PackedButtonRow(
              buttons: [
                PackedButton(
                  debugLabel: "New Output",
                  child: const Icon(CupertinoIcons.add)
                      .padding(const EdgeInsets.all(4)),
                  colour: ColourPalette.of(context).active,
                  filledChildColour:
                      ColourPalette.of(context).secondaryBackground,
                  onTap: () => widget.updateStream.publish(Update(
                    programme: programme.withDefaultSettings(
                      programme.defaultSettings.rebuild(
                        (settingsBuilder) => settingsBuilder
                            .addAll({"New": const DisplaySettings.default_()}),
                      ),
                    ),
                  )),
                )
              ].toBuiltList(),
              padding: const EdgeInsets.all(1),
            ),
          ],
        ).container(
          padding: const EdgeInsets.all(20),
          color: ColourPalette.of(context).secondaryBackground,
        ),
        DisplaySettingsPanel(
          displaySettings: programme.defaultSettings,
          update: (settings) => widget.updateStream.publish(
            Update(programme: programme.withDefaultSettings(settings)),
          ),
        ).background(ColourPalette.of(context).background).expanded(),
      ],
    );
  }
}
